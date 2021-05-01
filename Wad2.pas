unit Wad2;

interface

uses System.Classes, System.Generics.Collections, System.Math.Vectors, FMX.Types3D, FMX.Objects3D;

type
  TPoly = record
    p1,p2,p3,p4 : UInt32;
  end;

  TWadMesh = record
    verts : TList<TPoint3D>;
    normals : TList<TPoint3D>;
    quads : TList<TPoly>;
    tris  : TList<TPoly>;
  end;

  TMoveable = record
    slot : UInt32;
    meshes : TList<TWadMesh>;
  end;

  TWAD2 = record
    magic : string;
    version : UInt32;
    compressed : Boolean;
    moveables : TList<TMoveable>;
  end;

function LoadWad2(const stream:TMemoryStream; var w :TWAD2):Boolean;
function ConvertMesh(const msh : TWadMesh) : TMesh;
function ConvertMesh2(const msh : TWadMesh) : TMeshData;
procedure FreeWad(var w : TWAD2);

implementation

uses System.SysUtils, LEB128;

function GetChunkId(const br:TBinaryReader; const chunksize: Integer) : string;
var
  i:Integer;
  ss:string;
begin
    ss := '';
    for i := 1 to chunkSize do
      ss := ss + Chr(br.ReadByte);
    Result := ss;
end;

function ReadChunks(const stream:TMemoryStream; var w:TWAD2):Integer;
var
  ss:string;
  chunkSize,  chunkStart,  readDataCount  : Int64;
  chunkSize2, chunkstart2, readdatacount2 : Int64;
  chunkSize3, chunkstart3, readdatacount3 : Int64;
  chunkSize4, chunkstart4, readdatacount4 : Int64;
  chunkSize5, chunkstart5, readdatacount5 : Int64;
  br : TBinaryReader;
  mov : TMoveable;
  msh : TWadMesh;
  poly : TPoly;
  v , n :TPoint3D;
begin
  Result := -1;
  br := TBinaryReader.Create(stream);
  while (True) do
  begin
    chunkSize := LEB128.ReadInt(br);
    ss := GetChunkId(br, chunkSize);
    if chunkSize = 0 then Break;
    chunkSize := LEB128.ReadInt(br);
    chunkStart := stream.Position;
    if ss = 'W2Moveables' then
    begin
      while True do
      begin
        chunkSize2 := LEB128.ReadInt(br);
        if chunkSize2 = 0 then Break;
        ss := GetChunkId(br, chunkSize2);
        chunkSize2 := LEB128.ReadInt(br);
        chunkstart2 := stream.Position;
        if ss = 'W2Moveable' then
        begin
          mov.slot := LEB128.ReadUint(br); //typeId
          mov.meshes := TList<TWadMesh>.Create;
          while True do
          begin
            chunksize3 := LEB128.ReadInt(br);
            if chunkSize3 = 0 then Break;
            ss := GetChunkId(br, chunksize3);
            chunkSize3 := LEB128.ReadInt(br);
            chunkStart3 := stream.Position;
            if ss = 'W2Mesh' then
            begin
              msh.verts := TList<TPoint3D>.Create;
              msh.normals := TList<TPoint3D>.Create;
              msh.quads := TList<TPoly>.Create;
              msh.tris := TList<TPoly>.Create;
              while True do
              begin
                chunksize4 := LEB128.ReadInt(br);
                if chunkSize4 = 0 then Break;
                ss := GetChunkId(br, chunkSize4);
                chunkSize4 := LEB128.ReadInt(br);
                chunkStart4 := stream.Position;
                if (ss = 'W2VrtPos') or (ss = 'W2VrtNorm') or (ss = 'W2Polys') then
                begin
                  while True do
                  begin
                    chunkSize5 := LEB128.ReadInt(br);
                    if chunkSize5 = 0 then Break;
                    ss := GetChunkId(br, chunkSize5);
                    chunkSize5 := LEB128.ReadInt(br);
                    chunkStart5 := stream.Position;
                    if ss = 'W2Pos' then
                    begin
                      v:= TPoint3D.Create(
                      br.ReadSingle,
                      br.ReadSingle,
                      br.ReadSingle);
                      msh.verts.Add(v);
                    end
                    else if ss = 'W2N' then
                    begin
                      n := TPoint3D.Create(
                      br.ReadSingle,
                      br.ReadSingle,
                      br.ReadSingle);
                      msh.normals.Add(n);
                    end
                    else if (ss = 'W2Tr') or (ss = 'W2Uq') then
                    begin
                      poly.p1 := LEB128.ReadInt(br);
                      poly.p2 := LEB128.ReadInt(br);
                      poly.p3 := LEB128.ReadInt(br);
                      if ss = 'W2Uq' then poly.p4 := LEB128.ReadInt(br);
                      LEB128.ReadByte(br); //shine
                      LEB128.ReadInt(br); //texture index
                      br.ReadSingle; // u0
                      br.ReadSingle; // v0
                      br.ReadSingle; // u1
                      br.ReadSingle; // v1
                      br.ReadSingle; // u2
                      br.ReadSingle; // v2
                      if ss = 'W2Uq' then
                      begin
                        br.ReadSingle; // u3
                        br.ReadSingle; // v3
                      end;
                      ReadLEB128(br); //blendmode
                      br.ReadBoolean; //doublesided
                      // skip the terminating byte = 0x00
                      readDataCount5 := stream.Position - chunkStart5;
                      if readDataCount5 <> chunkSize5 then stream.Position := chunkstart5 + chunksize5;
                      if ss = 'W2Uq' then
                        msh.quads.Add(poly)
                      else
                        msh.tris.Add(poly);
                    end
                    else
                    begin
                      readDataCount5 := stream.Position - chunkStart5;
                      if readDataCount5 <> chunkSize5 then stream.Position := chunkstart5 + chunksize5;
                    end;
                  end;
                end
                else
                begin
                  readDataCount4 := stream.Position - chunkStart4;
                  if readDataCount4 <> chunkSize4 then stream.Position := chunkstart4 + chunksize4;
                end;
              end;
              mov.meshes.Add(msh);
            end
            else
            begin
              readDataCount3 := stream.Position - chunkStart3;
              if readDataCount3 <> chunkSize3 then stream.Position := chunkStart3 + chunkSize3;
            end;
          end;
          w.moveables.Add(mov);
        end
        else
        begin
          readDataCount2 := stream.Position - chunkStart2;
          if readDataCount2 <> chunkSize2 then stream.Position := chunkStart2 + chunkSize2;
        end;
      end;
    end
    else
    begin
      readDataCount := stream.Position - chunkStart;
      if readDataCount <> chunkSize then stream.Position := chunkStart + chunkSize;
    end;
  end;
  br.Free;
  Result := 0;
end;

function LoadWad2(const stream:TMemoryStream; var w : TWAD2):Boolean;
var
  br : TBinaryReader;
  i, resultado : Integer;
  ss : string;
begin
  Result := False;
  stream.Position := 0;
  br:=TBinaryReader.Create(stream);
  FreeWad(w);
  w.moveables := TList<TMoveable>.Create;
  ss:='';
  for i := 1 to 4 do
    ss := ss + Chr(br.ReadByte);
  w.magic := ss;
  if w.magic <> 'WAD2' then Exit;
  w.version := br.ReadUInt32;
  w.compressed := (w.version = $80000000);
  if w.compressed then Exit;
  resultado := ReadChunks(stream, w);
  if resultado <> 0 then Exit;
  Result := True;
  br.Free;
end;

function ConvertMesh(const msh : TWadMesh):TMesh;
// convert WadMesh to FMX TMesh
var
  i : Integer;
  v : TPoint3D;
  t : TPoly;
  verts : array of TMeshVertex;
  tris : array of UInt32;
  scale : Single;
begin
  Result := TMesh.Create(nil);
  SetLength(verts, msh.verts.Count);
  i := 0;
  scale := 0.005;
  for v in msh.verts do
  begin
    verts[i].x := v.X * scale;     //rotate about axis
    verts[i].y := -v.Y * scale;
    verts[i].z := -v.Z * scale;
    Inc(i);
  end;
  SetLength(tris, msh.tris.Count * 3 + msh.quads.Count * 2 * 3);
  i := 0;
  for t in msh.tris do
  begin
    tris[i]   := t.p3;  // reverse winding
    tris[i+1] := t.p2;
    tris[i+2] := t.p1;
    Inc(i, 3);
  end;
  for t in msh.quads do
  begin
    tris[i]   := t.p3;     // reverse winding
    tris[i+1] := t.p2;
    tris[i+2] := t.p1;
    tris[i+3] := t.p1;
    tris[i+4] := t.p4;
    tris[i+5] := t.p3;
    Inc(i, 6);
  end;
  Result.Data.AssignFromMeshVertex(verts, tris);
  Result.Data.CalcSmoothNormals();
end;

function ConvertMesh2(const msh : TWadMesh):TMeshData;
// convert indices to pairs of start and endpoints for drawlines
var
  i : Integer;
  v : TPoint3D;
  t : TPoly;
  scale : Single;
begin
  Result := TMeshData.Create;
  Result.VertexBuffer.Create([TVertexFormat.Vertex], msh.verts.Count);
  i := 0;
  scale := 0.005;
  for v in msh.verts do
  begin
    Result.VertexBuffer.Vertices[i] := TPoint3D.Create(v.X * scale, -v.Y * scale, -v.Z * scale);
    Inc(i);
  end;
  Result.IndexBuffer.Create(msh.tris.Count * 6 + msh.quads.Count * 8, TIndexFormat.UInt32);
  i := 0;
  for t in msh.tris do
  begin
    Result.IndexBuffer[i]   := t.p1;
    Result.IndexBuffer[i+1] := t.p2;
    Result.IndexBuffer[i+2] := t.p2;
    Result.IndexBuffer[i+3] := t.p3;
    Result.IndexBuffer[i+4] := t.p3;
    Result.IndexBuffer[i+5] := t.p1;
    Inc(i, 6);
  end;
  for t in msh.quads do
  begin
    Result.IndexBuffer[i]   := t.p1;
    Result.IndexBuffer[i+1] := t.p2;
    Result.IndexBuffer[i+2] := t.p2;
    Result.IndexBuffer[i+3] := t.p3;
    Result.IndexBuffer[i+4] := t.p3;
    Result.IndexBuffer[i+5] := t.p4;
    Result.IndexBuffer[i+6] := t.p4;
    Result.IndexBuffer[i+7] := t.p1;
    Inc(i, 8);
  end;
end;

procedure FreeWad(var w : TWAD2);
var
  m : TMoveable;
  msh : TWadMesh;

begin
  if Assigned(w.moveables) then
  begin
    for m in w.moveables do
    begin
      for msh in m.meshes do
      begin
        msh.verts.Free;
        msh.normals.Free;
        if Assigned(msh.quads) then msh.quads.Free;
        if Assigned(msh.tris)  then msh.tris.Free;
      end;
      m.meshes.Free;
    end;
    w.moveables.Free;
  end;
end;

end.