unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Viewport3D,
  System.Math.Vectors, FMX.Types3D, FMX.Controls3D, FMX.Objects3D,
  FMX.MaterialSources, FMX.Gestures, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Menus, System.Actions, FMX.ActnList, FMX.StdActns, FMX.Layouts,
  FMX.TreeView,
  Wad2, FMX.Ani, FMX.Effects, FMX.ListBox, FMX.Edit, FMX.ComboEdit;

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
    ToolBar1: TToolBar;
    GridLayout1: TGridLayout;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Timer1: TTimer;
    Label8: TLabel;
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
    procedure DummyVertsRender(Sender: TObject; Context: TContext3D);
    procedure SaveAsClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FDown: TPointF;
    FMouseS : TShiftState;
    FSelectedVert : Integer;
    FmovIdx, FmshIdx : Integer;
    procedure DoZoom(aIn: boolean);
    procedure SwapVerts(v1, v2 : Integer);
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
  ZOOM_STEP = 1;
  PAN_STEP = 0.01;
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
  CameraZ.Position.X := 0;
  CameraZ.Position.Y := 0;
  DummyXY.RotationAngle.X := 0;
  DummyXY.RotationAngle.Y := 0;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  if ComboBox1.ItemIndex < 0 then Exit;
  if ComboBox2.ItemIndex < 0 then Exit;
  if ComboBox1.ItemIndex = ComboBox2.ItemIndex then Exit;
  SwapVerts(ComboBox1.ItemIndex, ComboBox2.ItemIndex);
  ComboBox1Change(Sender);
  Timer1.Enabled := True;
  Label7.Visible := True;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  msh : TWadMesh;
  v : TVert;
begin
  if ComboBox1.Items.Count = 0 then Exit;
  FSelectedVert := ComboBox1.ItemIndex;
  Label1.Text := '#' + FSelectedVert.ToString + ' selected';
  msh := w.moveables[FmovIdx].meshes[FmshIdx];
  v := msh.verts[FSelectedVert];
  Label2.Text := Format('x: %.0f y: %.0f z: %.0f',[v.coords.X, v.coords.Y, v.coords.Z]);
  Viewport3DMain.Repaint;
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
  if (ssLeft in Shift)  and (ssCtrl in Shift) and (FSelectedVert > -1)then
  begin
    SwapVerts(FSelectedVert, p.Tag);
    Label2.Text := Format('x: %.0f y: %.0f z: %.0f',[p.Position.X*scale, -p.Position.Y*scale, -p.Position.Z*scale]);
  end
  else
  if ssLeft in Shift then
  begin
    Label1.Text := '#' + p.Tag.ToString + ' selected';
    Label2.Text := Format('x: %.0f y: %.0f z: %.0f',[p.Position.X*scale, -p.Position.Y*scale, -p.Position.Z*scale]);
    FSelectedVert := p.Tag;
    ComboBox1.ItemIndex := FSelectedVert;
  end;
  DummyVerts.Repaint;
end;


procedure TForm1.DummyVertsRender(Sender: TObject; Context: TContext3D);
var
  i : Integer;
begin
  if FSelectedVert < 0 then Exit;
  Context.BeginScene;
  for i := 0 to DummyVerts.ChildrenCount-1 do
  begin
    if DummyVerts.Children[i].Tag < 0 then Continue;
    if DummyVerts.Children[i].Tag = FSelectedVert then
    begin
      TProxyObject(DummyVerts.Children[i]).Scale.X := 2;
      TProxyObject(DummyVerts.Children[i]).Scale.y := 2;
      TProxyObject(DummyVerts.Children[i]).Scale.z := 2;
    end
    else
    begin
      TProxyObject(DummyVerts.Children[i]).Scale.X := 1;
      TProxyObject(DummyVerts.Children[i]).Scale.y := 1;
      TProxyObject(DummyVerts.Children[i]).Scale.z := 1;
    end;
  end;
  Context.EndScene;
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
  TreeView1.Clear;
  Label1.Text := '';
  Label2.Text := '';
  Label3.Text := '';
  Label4.Text := '';
  label7.Visible := False;
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
end;

procedure TForm1.FormResize(Sender: TObject);
begin
//  GridLayout1.ItemWidth := Viewport3DMain.Width / 5.0;
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
    if Assigned(md) then md.Clear;
    Mesh1.Data.Clear;
    FSelectedVert := -1;
    // Next line is hack needed to avoid access violation in Fmx.controls3d when sometimes open new wad2.
    // Cause unknown??? Occurs when high number of vertices. renderinglist count is not zero even though no children
    DummyVerts.Visible := False;
    DummyVerts.DeleteChildren;
    Label1.Text := '';
    Label2.Text := '';
    Label3.Text := '';
    Label4.Text := '';
    ComboBox1.Items.Clear;
    ComboBox2.Items.Clear;
    Button1Click(Sender);
    Viewport3DMain.Repaint;
  end;
end;


procedure TForm1.SaveAsClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  if w.moveables.Count > 0 then
  begin
    memstream.SaveToFile('C:/Users/Username/Documents/swapwad.wad2');
    ShowMessage('Saved');
  end;
{$ENDIF}
end;

procedure TForm1.SwapVerts(v1, v2: Integer);
const
  aux = $DeadBeef;
var
  i : Integer;
  va,vb,v : TVert;
  msh : TWadMesh;
  p :TPoly;
  po, po2 : TProxyObject;
  src : TControl3D;
  bw : TBinaryWriter;
  byteCount : Integer;
begin
  if v1 = v2 then Exit;
  bw := TBinaryWriter.Create(memstream);
  msh := w.moveables[FmovIdx].meshes[FmshIdx];
  va := msh.verts[v1];
  vb := msh.verts[v2];
  v  := msh.verts[v1];
  v.coords := vb.coords;
  w.moveables[FmovIdx].meshes[FmshIdx].verts[v1] := v;
  memstream.Position := v.address;
  bw.Write(v.coords.X);
  bw.Write(v.coords.Y);
  bw.Write(v.coords.Z);
  v := msh.verts[v2];
  v.coords := va.coords;
  w.moveables[FmovIdx].meshes[FmshIdx].verts[v2] := v;
  memstream.Position := v.address;
  bw.Write(v.coords.X);
  bw.Write(v.coords.Y);
  bw.Write(v.coords.Z);
  bw.Free;
  for i := 0 to msh.tris.Count-1 do
  begin
    p := w.moveables[FmovIdx].meshes[FmshIdx].tris[i];
    if p.p1 = v1 then
    begin
      p.p1 := aux;
      w.moveables[FmovIdx].meshes[FmshIdx].tris[i] := p;
    end;
    if p.p2 = v1 then
    begin
      p.p2 := aux;
      w.moveables[FmovIdx].meshes[FmshIdx].tris[i] := p;
    end;
    if p.p3 = v1 then
    begin
      p.p3 := aux;
      w.moveables[FmovIdx].meshes[FmshIdx].tris[i] := p;
    end;
  end;

  for i := 0 to msh.tris.Count-1 do
  begin
    p := w.moveables[FmovIdx].meshes[FmshIdx].tris[i];
    if p.p1 = v2 then
    begin
      p.p1 := v1;
      w.moveables[FmovIdx].meshes[FmshIdx].tris[i] := p;
    end;
    if p.p2 = v2 then
    begin
      p.p2 := v1;
      w.moveables[FmovIdx].meshes[FmshIdx].tris[i] := p;
    end;
    if p.p3 = v2 then
    begin
      p.p3 := v1;
      w.moveables[FmovIdx].meshes[FmshIdx].tris[i] := p;
    end;
  end;

  for i := 0 to msh.tris.Count-1 do
  begin
    p := w.moveables[FmovIdx].meshes[FmshIdx].tris[i];
    if p.p1 = aux then
    begin
      p.p1 := v2;
      w.moveables[FmovIdx].meshes[FmshIdx].tris[i] := p;
    end;
    if p.p2 = aux then
    begin
      p.p2 := v2;
      w.moveables[FmovIdx].meshes[FmshIdx].tris[i] := p;
    end;
    if p.p3 = aux then
    begin
      p.p3 := v2;
      w.moveables[FmovIdx].meshes[FmshIdx].tris[i] := p;
    end;
  end;
  // quads
  for i := 0 to msh.quads.Count-1 do
  begin
    p := w.moveables[FmovIdx].meshes[FmshIdx].quads[i];
    if p.p1 = v1 then
    begin
      p.p1 := aux;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
    if p.p2 = v1 then
    begin
      p.p2 := aux;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
    if p.p3 = v1 then
    begin
      p.p3 := aux;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
    if p.p4 = v1 then
    begin
      p.p4 := aux;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
  end;

  for i := 0 to msh.quads.Count-1 do
  begin
    p := w.moveables[FmovIdx].meshes[FmshIdx].quads[i];
    if p.p1 = v2 then
    begin
      p.p1 := v1;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
    if p.p2 = v2 then
    begin
      p.p2 := v1;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
    if p.p3 = v2 then
    begin
      p.p3 := v1;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
    if p.p4 = v2 then
    begin
      p.p4 := v1;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
  end;

  for i := 0 to msh.quads.Count-1 do
  begin
    p := w.moveables[FmovIdx].meshes[FmshIdx].quads[i];
    if p.p1 = aux then
    begin
      p.p1 := v2;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
    if p.p2 = aux then
    begin
      p.p2 := v2;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
    if p.p3 = aux then
    begin
      p.p3 := v2;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
    if p.p4 = aux then
    begin
      p.p4:= v2;
      w.moveables[FmovIdx].meshes[FmshIdx].quads[i] := p;
    end;
  end;
  // Got to rewrite all polys because index swap may increase or decrease the poly chunk size
  // Indices < 64 are one byte, indices 64 to 8191 are two bytes
  // However polys chunk size should remain same I think  !!!wrong if number of faces affected unequal
  memstream := WritePolysChunk(w, FmovIdx, FmshIdx, memstream);
  for i := 0 to DummyVerts.ChildrenCount-1 do
  begin
    if DummyVerts.Children[i].Tag = v1 then po  := TProxyObject(DummyVerts.Children[i]);
    if DummyVerts.Children[i].Tag = v2 then po2 := TProxyObject(DummyVerts.Children[i]);
  end;

  po.Tag := v2;
  po2.Tag := v1;

  src := po.SourceObject;
  po.SourceObject := po2.SourceObject;
  po2.SourceObject := src;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label7.Visible := False;
  Timer1.Enabled := False;
end;

procedure TForm1.TreeView1Click(Sender: TObject);
var
  node : TTreeViewItem;
  movIdx, mshIdx : Integer;
  m : TMesh;
  msh : TWadMesh;
  i : Integer;
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
  FmovIdx := movIdx;
  FmshIdx := mshIdx;
  msh := w.moveables[movIdx].meshes[mshIdx];
  Label3.Text := msh.verts.Count.ToString + ' vertices';
  m := ConvertMesh(msh);
  Mesh1.Data.Assign(m.Data);
  m.Free;
  if Assigned(md) then  md.Clear;
  md.Free;
  TreeView1.Cursor := crHourGlass;
  md := ConvertMesh2(msh);
  DummyVerts.Visible := True;
  CreatePoints(msh, DummyVerts, MaterialSourceY, MaterialSourceX, DummyVertsMouseDown);
  FSelectedVert := -1;
  Label4.Text := '#'+ msh.vertlimit.ToString + ' max.';
  ComboBox1.Items.Clear;
  ComboBox2.Items.Clear;
  Label1.Text := '';
  Label2.Text := '';
  for i := 0 to msh.verts.Count-1 do
  begin
    ComboBox1.Items.Add(IntToStr(i));
    ComboBox2.Items.Add(IntToStr(i));
  end;
  ComboBox1.ItemIndex := -1;
  ComboBox2.ItemIndex := -1;
  TreeView1.Cursor := crDefault;
end;

procedure TForm1.Viewport3DMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FDown := PointF(X, Y);
  FMouseS := Shift;
end;

procedure TForm1.Viewport3DMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
{$IFDEF DEBUG}
  if (ssLeft in FMouseS) then
{$ELSE}
  if (ssRight in FMouseS) then
{$ENDIF}
  begin
    DummyXY.RotationAngle.X := DummyXY.RotationAngle.X - ((Y - FDown.Y) * ROTATION_STEP);
    DummyXY.RotationAngle.Y := DummyXY.RotationAngle.Y + ((X - FDown.X) * ROTATION_STEP);
    FDown := PointF(X, Y);
  end
  else if (ssMiddle in FMouseS) then
  begin
    CameraZ.Position.X := CameraZ.Position.X - (X - FDown.X) * PAN_STEP;
    CameraZ.Position.Y := CameraZ.Position.Y - (Y - FDown.Y) * PAN_STEP;
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
