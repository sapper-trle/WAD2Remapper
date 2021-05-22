unit LEB128;

interface

uses System.Classes;

const MAXIMUMSIZE4BYTE:Int64 = 64 * 128 * 128 * 128 - 1;

function ReadLEB128(const reader:TBinaryReader):Int64;
function ReadInt(const br:TBinaryReader):Integer;
function ReadShort(const br:TBinaryReader):Int16;
function ReadSByte(const br:TBinaryReader):Int8;
function ReadUint(const br:TBinaryReader):UInt32;
function ReadUShort(const br:TBinaryReader):UInt16;
function ReadByte(const br:TBinaryReader):UInt8;
function GetLength(value:Int64) :Int8;
procedure Write(var stream: TMemoryStream; value, maximumSize: Int64);

implementation

uses System.Math;

function GetLength(value:Int64) :Int8;
begin
  Result := 1;
  value := value shr 6;
  while value > 0 do
  begin
    value := value shr 7;
    Inc(Result);
  end;
end;

procedure Write(var stream: TMemoryStream; value, maximumSize: Int64);
var
  currentByte : UInt8;
  bw : TBinaryWriter;
begin
  bw := TBinaryWriter.Create(stream);
  while (True) do
  begin
    currentByte := value and $7F;
    if ((maximumSize shr 6) = 0) or ((maximumSize shr 6) = -1) then
    begin
      bw.Write(currentByte);
      bw.Free;
      Exit;
    end;
    bw.Write(currentByte or $80);
    value := value shr 7;
    maximumSize := maximumSize shr 7;
  end;
end;

function ReadLEB128(const reader:TBinaryReader):Int64;
var
  currentshift : Integer;
  currentbyte : Byte;
  shift : Integer;
begin
  Result := 0;
  currentshift := 0;
  repeat
    currentbyte := reader.ReadByte;
    Result := Result or (Int64((currentbyte and $7F) shl currentshift));
    currentshift := currentshift + 7;
  until ((currentbyte and $80)=0);
  shift := 64 - currentshift;
  if shift > 0 then
    Result := (Result shl shift) shr shift;
end;

function ReadInt(const br:TBinaryReader):Integer;
begin
  Result := Integer(Min(Max(ReadLEB128(br), Low(Integer)),High(Integer)));
end;

function ReadShort(const br:TBinaryReader):Int16;
begin
  Result := Int16(Min(Max(ReadLEB128(br), Low(Int16)),High(Int16)));
end;

function ReadSByte(const br:TBinaryReader):Int8;
begin
  Result := Int8(Min(Max(ReadLEB128(br), Low(Int8)),High(Int8)));
end;

function ReadUint(const br:TBinaryReader):UInt32;
begin
  Result := UInt32(Min(Max(ReadLEB128(br), 0),High(Uint32)));
end;

function ReadUShort(const br:TBinaryReader):UInt16;
begin
  Result := UInt16(Min(Max(ReadLEB128(br), 0),High(Uint16)));
end;

function ReadByte(const br:TBinaryReader):UInt8;
begin
  Result := UInt8(Min(Max(ReadLEB128(br), 0),High(Uint8)));
end;

end.
