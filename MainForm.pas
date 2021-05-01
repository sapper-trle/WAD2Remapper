unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Viewport3D,
  System.Math.Vectors, FMX.Types3D, FMX.Controls3D, FMX.Objects3D,
  FMX.MaterialSources, FMX.Gestures, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Menus, System.Actions, FMX.ActnList, FMX.StdActns, FMX.Layouts,
  FMX.TreeView;

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
  private
    FDown: TPointF;
//    FLastDistance: integer;
    procedure DoZoom(aIn: boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Wad2, FMX.DialogService;

{$R *.fmx}

const
  ROTATION_STEP = 0.6;
  ZOOM_STEP = 2;
  CAMERA_MAX_Z = -2;
  CAMERA_MIN_Z = -102;

  FORMCAPTION = 'WAD2 Remapper';

var
  w : TWAD2;
  memstream : TMemoryStream;

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
var
  msh : TWadMesh;
  m : TMesh;
  md : TmeshData;
begin
  if not Assigned(w.moveables) then Exit;
  if w.moveables.Count = 0 then Exit;
  msh := w.moveables[0].meshes[0];
  m := ConvertMesh(msh);
  md := ConvertMesh2(msh);
  Context.BeginScene;
  Mesh1.Data.Assign(m.Data);
  Context.DrawLines(md.VertexBuffer,md.IndexBuffer,ColorMaterialSource1.Material,1.0);
  Context.EndScene;
  m.Free;
  md.Free;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  memstream.Free;
  FreeWad(w);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  memstream := TMemoryStream.Create;
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
  i : Integer;
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
    for m in w.moveables do
    begin
      t := TTreeViewItem.Create(TreeView1);
      t.Text := Format('Moveable%d',[m.slot]);
      t.Parent:= TreeView1;
      i := 0;
      for msh in m.meshes do
      begin
        t2 := TTreeViewItem.Create(TreeView1);
        t2.Text := Format('Mesh%d',[i]);
        t2.Parent := t;
        inc(i);
      end;
    end;
    TreeView1.EndUpdate;
  {$ENDIF}
end;


procedure TForm1.OpenClick(Sender: TObject);
var
  m : TMoveable;
  msh : TWadMesh;
  T,t2 :TTreeViewItem;
  i : Integer;
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
    for m in w.moveables do
    begin
      t := TTreeViewItem.Create(TreeView1);
      t.Text := Format('Moveable%d',[m.slot]);
      t.Parent:= TreeView1;
      i := 0;
      for msh in m.meshes do
      begin
        t2 := TTreeViewItem.Create(TreeView1);
        t2.Text := Format('Mesh%d',[i]);
        t2.Parent := t;
        inc(i);
      end;
    end;
    TreeView1.EndUpdate;
  end;
end;

procedure TForm1.Viewport3DMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FDown := PointF(X, Y);
end;

procedure TForm1.Viewport3DMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  if (ssLeft in Shift) then
  begin
    DummyXY.RotationAngle.X := DummyXY.RotationAngle.X - ((Y - FDown.Y) * ROTATION_STEP);
    DummyXY.RotationAngle.Y := DummyXY.RotationAngle.Y + ((X - FDown.X) * ROTATION_STEP);
    FDown := PointF(X, Y);
  end;
end;

procedure TForm1.Viewport3DMainMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  DoZoom(WheelDelta > 0);
end;

end.
