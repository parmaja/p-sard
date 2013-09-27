unit main;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterCss, Forms, Controls,
  Graphics, Dialogs, StdCtrls,
  LCLType,
  sard;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    InputEdit: TSynEdit;
    SynCssSyn1: TSynCssSyn;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
  public
    procedure Run;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Run;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InputEdit.Lines.Text := '10 + ( 2 * (6 * 6 ) )';
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F9 then
    Run;
end;

procedure TForm1.Run;
begin
  Execute(InputEdit.Lines);
end;

end.

