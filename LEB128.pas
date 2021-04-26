unit LEB128;

interface

uses System.Classes;

function ReadLEB128(const reader:TBinaryReader):Int64;
function ReadInt(const br:TBinaryReader):Integer;
function ReadShort(const br:TBinaryReader):Int16;
function ReadSByte(const br:TBinaryReader):Int8;
function ReadUint(const br:TBinaryReader):UInt32;
function ReadUShort(const br:TBinaryReader):UInt16;
function ReadByte(const br:TBinaryReader):UInt8;

implementation

uses System.Math;

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
