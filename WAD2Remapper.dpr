program WAD2Remapper;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {FormSimpleCamera},
  LEB128 in 'LEB128.pas',
  Wad2 in 'Wad2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSimpleCamera, FormSimpleCamera);
  Application.Run;
end.
