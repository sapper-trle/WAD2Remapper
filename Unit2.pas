unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm2 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Memo1: TMemo;
    ImageControl1: TImageControl;
    ImageControl2: TImageControl;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
begin
  if FileExists('readme.txt') then
  begin
    Memo1.Lines.Clear;
    Memo1.Lines.LoadFromFile('readme.txt');
  end;
end;

end.
