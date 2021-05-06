unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Viewport3D,
  System.Math.Vectors, FMX.Types3D, FMX.Controls3D, FMX.Objects3D,
  FMX.MaterialSources, FMX.Gestures, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Menus, System.Actions, FMX.ActnList, FMX.StdActns, FMX.Layouts,
  FMX.TreeView,
  Wad2, FMX.Ani;

type
  TForm1 = class(TForm)
    Viewport3DMain: TViewport3D;
    DummyScene: TDummy;
    DummyXY: TDummy;
    CameraZ: TCamera;
    LightCamera: TLight;
    MaterialSourceY: TLightMaterialSource;
    MaterialSourceZ: TLightMaterialSource;
    MaterialSourceX: TLightMaterialSource;
    CylX: TCylinder;
    ConeX: TCone;
    CylY: TCylinder;
    ConeY: TCone;
    CylZ: TCylinder;
    ConeZ: TCone;
    StatusBar1: TStatusBar;
    Button1: TButton;
    MenuBar1: TMenuBar;
    MenuItem1: TMenuItem;
    Open: TMenuItem;
    SaveAs: TMenuItem;
    Quit: TMenuItem;
    ActionList1: TActionList;
    FileExit1: TFileExit;
    OpenDialog1: TOpenDialog;
    TreeView1: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    Mesh1: TMesh;
    LightMaterialSource1: TLightMaterialSource;
    ColorMaterialSource1: TColorMaterialSource;
    Light1: TLight;
    ColorMaterialSource2: TColorMaterialSource;
    Label1: TLabel;
    Label2: TLabel;
    DummyVerts: TDummy;
    Label3: TLabel;
    Label4: TLabel;
    procedure Viewport3DMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Viewport3DMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Viewport3DMainMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DummySceneRender(Sender: TObject; Context: TContext3D);
    procedure Viewport3DMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TreeView1Click(Sender: TObject);
    procedure DummyVertsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
  private
    FDown: TPointF;
    FMouseS : TShiftState;
    FSelectedVert : Integer;
    procedure DoZoom(aIn: boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses FMX.DialogService;

{$R *.fmx}

const
  ROTATION_STEP = 0.5;
  ZOOM_STEP = 2;
  CAMERA_MAX_Z = -1.5;
  CAMERA_MIN_Z = -52;

  FORMCAPTION = 'WAD2 Remapper';

var
  w : TWAD2;
  memstream : TMemoryStream;
  md : TMeshData;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CameraZ.Position.Z := -10;
  DummyXY.RotationAngle.X := 0;
  DummyXY.RotationAngle.Y := 0;
end;


procedure TForm1.DoZoom(aIn: boolean);
var newZ: single;
begin
  if aIn then
    newZ := CameraZ.Position.Z + ZOOM_STEP
  else
    newZ := CameraZ.Position.Z - ZOOM_STEP;

  if (newZ < CAMERA_MAX_Z) and (newZ > CAMERA_MIN_Z) then
    CameraZ.Position.Z := newZ;
end;


procedure TForm1.DummySceneRender(Sender: TObject; Context: TContext3D);
begin
  Context.BeginScene;
  try
    if Assigned(md) then
      Context.DrawLines(md.VertexBuffer,md.IndexBuffer,ColorMaterialSource1.Material,1.0);
  finally
    Context.EndScene;
  end;
end;

procedure TForm1.DummyVertsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
const
  scale = 1.0/0.005;
var
  p : TProxyObject;
  i : Integer;
begin
  p := TProxyObject(Sender);
  Label1.Text := p.Tag.ToString;
  Label2.Text := Format('x: %.0f y: %.0f z: %.0f',[p.Position.X*scale, -p.Position.Y*scale, -p.Position.Z*scale]);
  for i := 0 to DummyVerts.ChildrenCount-1 do
  begin
    if DummyVerts.Children[i].Tag = FSelectedVert then
    begin
      TProxyObject(DummyVerts.Children[i]).Scale.X := 1;
      TProxyObject(DummyVerts.Children[i]).Scale.y := 1;
      TProxyObject(DummyVerts.Children[i]).Scale.z := 1;
    end;
  end;
  FSelectedVert := p.Tag;
  p.Scale.X := 2;
  p.Scale.Y := 2;
  p.Scale.Z := 2;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  memstream.Free;
  FreeWad(w);
  md.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  size = 0.01;
begin
  memstream := TMemoryStream.Create;
  FMouseS := [];
  Mesh1.WrapMode := TMeshWrapMode.Resize; // important since default of Stretch will deform mesh
  CylY.Depth := size;
  CylY.Width := size;
  CylX.Depth := size;
  CylX.Width := size;
  CylZ.Depth := size;
  CylZ.Width := size;
  ConeX.Visible := False;
  ConeY.Visible := False;
  ConeZ.Visible := False;
  FSelectedVert := -1;
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
end;

procedure TForm1.FormShow(Sender: TObject);
const
  filename = '../../test.wad2';
var
  m : TMoveable;
  t, t2 : TTreeViewItem;
  i, j : Integer;
  msh : TWadMesh;
begin
{$IFDEF DEBUG}
  if not FileExists(filename) then Exit;
  memstream.loadfromfile(filename);
  if not LoadWad2(memstream, w) then
  begin
    TDialogService.MessageDialog('Unable to load file',TMsgDlgType.mtError,[TMsgDlgBtn.mbOK],TMsgDlgBtn.mbOK,0,nil);
    Caption := FORMCAPTION;
    Exit;
  end;
  Caption := FORMCAPTION + ' - Debug test.wad2';
    TreeView1.BeginUpdate;
    TreeView1.Clear;
    j := 0;
    for m in w.moveables do
    begin
      t := TTreeViewItem.Create(TreeView1);
      t.Text := Format('Moveable%d',[m.slot]);
      t.Tag := j;
      t.Parent:= TreeView1;
      i := 0;
      for msh in m.meshes do
      begin
        t2 := TTreeViewItem.Create(TreeView1);
        t2.Text := Format('[%d] %s',[i, msh.name]);
        t2.Tag := i;
        t2.Parent := t;
        inc(i);
      end;
      inc(j);
    end;
    TreeView1.EndUpdate;
{$ENDIF}
end;


procedure TForm1.OpenClick(Sender: TObject);
var
  m : TMoveable;
  msh : TWadMesh;
  T,t2 :TTreeViewItem;
  i, j : Integer;
begin
  if OpenDialog1.Execute then
  begin
    if (not FileExists(OpenDialog1.FileName)) then Exit;
    if LowerCase(ExtractFileExt(OpenDialog1.FileName)) <> '.wad2' then Exit;
    memstream.LoadFromFile(OpenDialog1.FileName);
    if not LoadWad2(memstream, w) then
    begin
      TDialogService.MessageDialog('Unable to load file',TMsgDlgType.mtError,[TMsgDlgBtn.mbOK],TMsgDlgBtn.mbOK,0,nil);
      memstream.Free;
      FreeWad(w);
      Caption := FORMCAPTION;
      Exit;
    end;
    Caption := FORMCAPTION + ' - ' + OpenDialog1.FileName;
    TreeView1.BeginUpdate;
    TreeView1.Clear;
    j := 0;
    for m in w.moveables do
    begin
      t := TTreeViewItem.Create(TreeView1);
      t.Text := Format('Moveable%d',[m.slot]);
      t.Tag := j;
      t.Parent:= TreeView1;
      i := 0;
      for msh in m.meshes do
      begin
        t2 := TTreeViewItem.Create(TreeView1);
        t2.Text := Format('[%d] %s',[i, msh.name]);
        t2.Tag := i;
        t2.Parent := t;
        inc(i);
      end;
      inc(j);
    end;
    TreeView1.EndUpdate;
  end;
end;

procedure TForm1.TreeView1Click(Sender: TObject);
var
  node : TTreeViewItem;
  movIdx, mshIdx : Integer;
  m : TMesh;
  msh : TWadMesh;
begin
  node := TreeView1.Selected;
  if (node.Level = 1) then
  begin
    if (node.Count > 0) then
    begin
      movIdx := node.Tag;
      node.Expand;
      node := node.Items[0];
      mshIdx := node.Tag;
      node.Select;
    end
    else
    begin
      // doubt this will ever be hit
      Mesh1.Data.Clear;
      md.Clear;
      Exit;
    end;
  end
  else
  begin
    mshIdx := node.Tag;
    movIdx := node.ParentItem.Tag;
  end;
  msh := w.moveables[movIdx].meshes[mshIdx];
  Label3.Text := msh.verts.Count.ToString;
  m := ConvertMesh(msh);
  Mesh1.Data.Assign(m.Data);
  m.Free;
  if Assigned(md) then  md.Clear;
  md.Free;
  md := ConvertMesh2(msh);
  CreatePoints(msh, DummyVerts, MaterialSourceY, MaterialSourceX, DummyVertsMouseDown);
  FSelectedVert := -1;
  Label4.Text := msh.vertlimit.ToString;
end;

procedure TForm1.Viewport3DMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FDown := PointF(X, Y);
//  Label1.Text := Format('%.4f %.4f', [x,y]);
  FMouseS := Shift;
end;

procedure TForm1.Viewport3DMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  if (ssLeft in FMouseS) then
  begin
    DummyXY.RotationAngle.X := DummyXY.RotationAngle.X - ((Y - FDown.Y) * ROTATION_STEP);
    DummyXY.RotationAngle.Y := DummyXY.RotationAngle.Y + ((X - FDown.X) * ROTATION_STEP);
    FDown := PointF(X, Y);
  end;
end;

procedure TForm1.Viewport3DMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  Viewport3DMainMouseMove(Sender, Shift, X, Y);  //luxophia
  FMouseS :=[];
end;

procedure TForm1.Viewport3DMainMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  DoZoom(WheelDelta > 0);
end;

end.
