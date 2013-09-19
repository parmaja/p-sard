unit main;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterCss, Forms, Controls,
  Graphics, Dialogs, StdCtrls,
  sard;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    InputEdit: TSynEdit;
    SynCssSyn1: TSynCssSyn;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Execute(InputEdit.Lines);
end;

end.

