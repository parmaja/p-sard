unit sardScripts;
{**
 *  This file is part of the "SARD"
 *
 * @license   Apache License Version 2.0 (modified of http://www.gnu.org/licenses/lgpl.html)
 *            included in this distribution,
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
  Sard;

const
  IDENTIFIER_OPEN_CHARS = ['A'..'Z', 'a'..'z', '_'];
  IDENTIFIER_CHARS = IDENTIFIER_OPEN_CHARS + ['0'..'9', '.']; //for : xdebug send tag like xdebug:message
  NUMBER_OPEN_CHARS = ['0'..'9'];
  NUMBER_CHARS = NUMBER_OPEN_CHARS + ['.','E'];
  OPERATOR_OPEN_CHARS = ['+', '-', '*', '/', '^', '&', '|', '!'];
  OPERATOR_CHARS = OPERATOR_OPEN_CHARS {+ ['']};
  SYMBOL_CHARS = ['$', '#', '@', '!', '\'];
  BRACKET_OPEN_CHARS = ['(', '[', '{'];
  BRACKET_CLOSE_CHARS= [')', ']', '}'];
  BRACKET_CHARS = BRACKET_OPEN_CHARS + BRACKET_CLOSE_CHARS;
  CONTROLS_OPEN_CHARS = [':', '~', ';', ','];
  CONTROLS_CHARS = CONTROLS_OPEN_CHARS + ['='];
  sSardAnsiOpen = '{?sard '; //with the space

type
  TsardTokenKinds = (tknOperator, tknControl, tknBracket, tknIdentifier, tknString, tknNumber, tknComment);

  { TsardScript }

  TsardScript = class(TsardFeeder)
  private
    FIntoElement: TsardElement;
  protected
    function CreateParser: TsardParser; override;
  public
    constructor Create; override;
    property IntoElement: TsardElement read FIntoElement write FIntoElement;
  end;

  { TsardScriptParser }

  TsardScriptParser = class(TsardParser)
  protected
    procedure Open(vBracket: sardBracketKind); override;
    procedure Close(vBracket: sardBracketKind); override;
    procedure Terminate; override;
    procedure Push(Token: String; TokenID: Integer); override;
  end;

  { TsardStartScanner }

  TsardStart_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardHeaderScanner }

  TsardHeader_Scanner = class(TsardScanner)
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

procedure TsardScriptParser.Open(vBracket: sardBracketKind);
begin
end;

procedure TsardScriptParser.Close(vBracket: sardBracketKind);
begin
end;

procedure TsardScriptParser.Terminate;
begin
end;

procedure TsardScriptParser.Push(Token: String; TokenID: Integer);
begin
end;

{ TsardControlScanner }

procedure TsardControl_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  b: AnsiChar;
begin
  b := Text[Column];
  if b = ';' then //TODO need to improve it
    Terminate
  else if b = ',' then
    Terminate;
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
    Open(brParenthesis)
  else if b = '[' then
    Open(brSquare)
  else if b = '{' then
    Open(brCurly)
  else if b = ')' then
    Close(brParenthesis)
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
    RegisterScanner(TsardHeader_Scanner);
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
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
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
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
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
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
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
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
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
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
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
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
end;

function TsardBlockComment_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('/*', Text, Column);
end;

{ TsardSLCommentScanner }

procedure TsardLineComment_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  while (Column <= Length(Text)) and not (Text[Column] in sEOL) do //TODO ignore quoted strings
    Inc(Column);
  //Push(MidStr(Text, c, Column - c)); ignore comment
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
end;

function TsardLineComment_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('//', Text, Column);
end;

{ TsardHeaderCommentScanner }

procedure TsardHeader_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (Text[Column] <> '}') do
    Inc(Column);
  Push(MidStr(Text, c, Column - c), 0);
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
end;

function TsardHeader_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False; //Only when file started, or first, so not accepted
end;

{ TsardNormalCommentScanner }

procedure TsardWhitespace_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  while (Column <= Length(Text)) and (Text[Column] in sWhitespace) do
    Inc(Column);
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
end;

function TsardWhitespace_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in sWhitespace;
end;

{ TsardStartScanner }

procedure TsardStart_Scanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  if LeftStr(Text, Length(sSardAnsiOpen)) = sSardAnsiOpen then
  begin
    //There is a header and it is a Ansi document
    Column := Column + Length(sSardAnsiOpen); //put the column to the first char of attributes of document
    SelectScanner(TsardHeader_Scanner);
  end
  else
    ChooseScanner(Text, Column, Line); //nop there is no header... skip to normal
end;

function TsardStart_Scanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False;//Start not accept the scan, it is only when starting scan
end;

end.
