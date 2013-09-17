unit main;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterCss, Forms, Controls,
  Graphics, Dialogs, StdCtrls,
  sard, sardScripts;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    InputEdit: TSynEdit;
    ResultEdit: TSynEdit;
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

type

  { TmyScanner }

  TmyParser = class(TsardScriptParser)
  protected
    procedure Push(Token: String; TokenID: Integer); override;
  end;

  { TmyScript }

  TmyScript = class(TsardScript)
  protected
    function CreateParser: TsardParser; override;
  end;

{ TmyScript }

function TmyScript.CreateParser: TsardParser;
begin
  Result := TmyParser.Create;
end;

{ TmyScanner }

procedure TmyParser.Push(Token: String; TokenID: Integer);
begin
  inherited;
  Form1.ResultEdit.Lines.Add(Token);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Scanner: TmyScript;
begin
  ResultEdit.Lines.Clear;
  Scanner := TmyScript.Create;
  try
    Scanner.Scan(InputEdit.Lines);
  finally
    Scanner.Free;
  end;
end;

end.

