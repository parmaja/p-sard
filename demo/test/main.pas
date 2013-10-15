unit main;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterCss, Forms, Controls,
  Graphics, Dialogs, StdCtrls,
  LCLType, Menus,
  sard;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    InputEdit: TSynEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    ResultEdit: TMemo;
    SynCssSyn1: TSynCssSyn;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItem4Click(Sender: TObject);
  private
  public
    procedure Save;
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

procedure TForm1.Button2Click(Sender: TObject);
begin
  Save;
  Build(InputEdit.Lines);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if FileExistsUTF8(Application.Location + 'recent.sard') then
    InputEdit.Lines.LoadFromFile(Application.Location + 'recent.sard')
  else
    InputEdit.Lines.Text := '10 + ( 2 * (6 * 6))';
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F9 then
    Run
  else if ssCtrl in Shift then
    begin
      if Key = VK_S then
        Save
      else if Key = VK_F9 then
        Build(InputEdit.Lines);
    end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  InputEdit.Lines.LoadFromFile(Application.Location + '../../examples/full_test.sard')
end;

procedure TForm1.Save;
begin
  InputEdit.Lines.SaveToFile(Application.Location + 'recent.sard');
end;

procedure TForm1.Run;
var
  s:string;
begin
  ResultEdit.Text := '';
  Save;
  Execute(InputEdit.Lines,s);
  ResultEdit.Text := s;
end;

end.

