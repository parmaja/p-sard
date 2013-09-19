unit sardScripts;
{**
 *  This file is part of the "SARD"
 *
 * @license   Apache License Version 2.0
 *            included in this distribution
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}{$M+}

(* Example

object: {
  v1:=10+10;
  v2:int=10;
  v3:=v2-v1;
  =proc1(v3);
};

*)

interface

uses
  Classes, SysUtils,
  sardClasses, sardObjects;

const
  IDENTIFIER_OPEN_CHARS = ['A'..'Z', 'a'..'z', '_'];
  IDENTIFIER_CHARS = IDENTIFIER_OPEN_CHARS + ['0'..'9', '.']; //for : xdebug send tag like xdebug:message
  NUMBER_OPEN_CHARS = ['0'..'9'];
  NUMBER_CHARS = NUMBER_OPEN_CHARS + ['.', 'x', 'h', 'a'..'f'];
  COLOR_OPEN_CHARS = ['#'];
  COLOR_CHARS = COLOR_OPEN_CHARS + ['0'..'9', 'a'..'f'];
  OPERATOR_OPEN_CHARS = ['+', '-', '*', '/', '^', '&', '|', '!', '='];
  OPERATOR_CHARS = OPERATOR_OPEN_CHARS {+ ['']};
  SYMBOL_CHARS = ['$', '#', '@', '!', '\'];
  BRACKET_OPEN_CHARS = ['(', '[', '{'];
  BRACKET_CLOSE_CHARS= [')', ']', '}'];
  BRACKET_CHARS = BRACKET_OPEN_CHARS + BRACKET_CLOSE_CHARS;
  CONTROLS_OPEN_CHARS = [':', '~', ';', ','];
  CONTROLS_CHARS = CONTROLS_OPEN_CHARS + ['='];

type
  TsardTokenKinds = (tknOperator, tknControl, tknBracket, tknIdentifier, tknString, tknNumber, tknColor, tknComment);

  { TsardScript }

  TsardScript = class(TsardFeeder)
  private
  protected
    function CreateParser: TsardParser; override;
  public
    constructor Create; override;
  end;

  { TsardScriptParser }

  TsardScriptParser = class(TsardParser)
  protected
    Block: TsardBlock;
    procedure Open(vBracket: TsardBracketKind); override;
    procedure Close(vBracket: TsardBracketKind); override;
    procedure Push(Token: String; TokenID: Integer); override;
  end;

  { TsardStartScanner }

  TsardStart_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardWhitespaceScanner }

  TsardWhitespace_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardIdentifierScanner }

  TsardIdentifier_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardNumberScanner }

  TsardNumber_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardBracketScanner }

  TsardBracket_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardTerminalScanner }

  TsardControl_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardOperatorScanner }

  TsardOperator_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardLineCommentScanner }

  TsardLineComment_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardBlockCommentScanner }

  TsardBlockComment_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardSQStringScanner }

  TsardSQString_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardDQStringScanner }

  TsardDQString_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

implementation

uses
  StrUtils;

function IsIdentifier(vChar: AnsiChar): Boolean;
begin
  Result := vChar in IDENTIFIER_CHARS;
end;

function IsOperator(vChar: AnsiChar): Boolean;
begin
  Result := vChar in OPERATOR_CHARS;
end;

{ TsardScriptParser }

procedure TsardScriptParser.Open(vBracket: TsardBracketKind);
begin
end;

procedure TsardScriptParser.Close(vBracket: TsardBracketKind);
begin
end;

procedure TsardScriptParser.Push(Token: String; TokenID: Integer);
  procedure AddObject(AnObject: TsardObject);
  begin

  end;
begin
  case TsardTokenKinds(TokenID) of
    tknNumber:
    begin
      if pos('.', Token) > 0 then
        TsardFloatObject.Create(nil).Value := StrToFloat(Token)
      else
        TsardIntegerObject.Create(nil).Value := StrToInt64(Token);//check it
    end;
    tknString:
    begin
      TsardStringObject.Create(nil).Value := Token;
    end;
  end;
end;

{ TsardControlScanner }

procedure TsardControl_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  b: AnsiChar;
begin
  b := Text[Column];
  if b = ';' then //TODO need to improve it
    Control(ctlFinish)
  else if b = ',' then
    Control(ctlSplit)
  else if b = ':' then
    Control(ctlDeclare)
  else if b = '~' then
    Control(ctlLink);
  Inc(Column);
end;

function TsardControl_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in CONTROLS_OPEN_CHARS;
end;

{ TsardBracketScanner }

procedure TsardBracket_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  b: AnsiChar;
begin
  b := Text[Column];
  if b = '(' then //TODO need to improve it, my brain is off
    Open(brBracket)
  else if b = '[' then
    Open(brSquare)
  else if b = '{' then
    Open(brCurly)
  else if b = ')' then
    Close(brBracket)
  else if b = ']' then
    Close(brSquare)
  else if b = '}' then
    Close(brCurly);
  Inc(Column);
end;

function TsardBracket_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in BRACKET_CHARS;
end;

{ TsardScript }

function TsardScript.CreateParser: TsardParser;
begin
  Result := TsardScriptParser.Create;
end;

constructor TsardScript.Create;
begin
  inherited Create;

  with Scanners do
  begin
    RegisterScanner(TsardStart_Scanner);
    RegisterScanner(TsardWhitespace_Scanner);
    RegisterScanner(TsardIdentifier_Scanner);
    RegisterScanner(TsardNumber_Scanner);
    RegisterScanner(TsardBracket_Scanner);
    RegisterScanner(TsardControl_Scanner);
    RegisterScanner(TsardSQString_Scanner);
    RegisterScanner(TsardDQString_Scanner);
    RegisterScanner(TsardBlockComment_Scanner);
    RegisterScanner(TsardLineComment_Scanner);
    RegisterScanner(TsardOperator_Scanner); //Register it after comment because comment take /*
  end;
  //FOffScanner := scanIdentifier;
end;

{ TsardNumberScanner }

procedure TsardNumber_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  l, c: Integer;
begin
  c := Column;
  l := Length(Text);
  while (Column <= l) and (Text[Column] in NUMBER_CHARS) do
    Inc(Column);
  Push(MidStr(Text, c, Column - c), Ord(tknNumber));
end;

function TsardNumber_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in NUMBER_OPEN_CHARS;//need to improve to accept unicode chars
end;

{ TsardOperatorScanner }

procedure TsardOperator_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (IsOperator(Text[Column])) do //operator is multi char here
    Inc(Column);
  Push(MidStr(Text, c, Column - c), Ord(tknOperator));
end;

function TsardOperator_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := IsOperator(Text[Column]);
end;

{ TsardIdentifierScanner }

procedure TsardIdentifier_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (Text[Column] in IDENTIFIER_CHARS) do
    Inc(Column);
  Push(MidStr(Text, c, Column - c), Ord(tknIdentifier));
end;

function TsardIdentifier_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in IDENTIFIER_OPEN_CHARS;//need to improve to accept unicode chars
end;

{ TsardDQStringScanner }

procedure TsardDQString_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '"') do //TODO Escape, not now
    Inc(Column);
  Push(MidStr(Text, c, Column - c), Ord(tknString));
end;

function TsardDQString_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('"', Text, Column);
end;

{ TsardSQStringScanner }

procedure TsardSQString_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '''') do //TODO Escape, not now
    Inc(Column);
  Push(MidStr(Text, c, Column - c), Ord(tknString));
end;

function TsardSQString_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('''', Text, Column);
end;

{ TsardBlockCommentScanner }

procedure TsardBlockComment_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  while (Column <= Length(Text)) do
  begin
    if (CheckText('*/', Text, Column)) then
    begin
      Inc(Column, 2);//2 chars
      break;
    end;
    Inc(Column);
  end;
end;

function TsardBlockComment_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('/*', Text, Column);
end;

{ TsardLineComment_Scanner }

procedure TsardLineComment_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  while (Column <= Length(Text)) and not (Text[Column] in sEOL) do //TODO ignore quoted strings
    Inc(Column);
  //Push(MidStr(Text, c, Column - c)); ignore comment
end;

function TsardLineComment_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('//', Text, Column);
end;

{ TsardWhitespace_Scanner }

procedure TsardWhitespace_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  while (Column <= Length(Text)) and (Text[Column] in sWhitespace) do
    Inc(Column);
end;

function TsardWhitespace_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in sWhitespace;
end;

{ TsardStart_Scanner }

procedure TsardStart_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
end;

function TsardStart_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False;//Start not accept the scan, it is only when starting scan
end;

end.
