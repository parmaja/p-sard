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

{ TsardScanner }

  TsardScript = class(TsardFeeder)
  private
    //       <--------- i don't know why i save it in variables, so it is temporary solution
    prcStart: Integer;
    prcHeader: Integer;
    prcWhiteSpace: Integer;
    prcIdentifier: Integer;
    prcNumber: Integer;
    prcBracket: Integer;
    prcOperator: Integer;
    prcControl: Integer;
    prcSQString: Integer;
    prcDQString: Integer;
    prcBlockComment: Integer;
    prcLineComment: Integer;
  protected
    function CreateParser: TsardParser; override;
  public
    constructor Create; override;
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

  TsardStartScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardHeaderScanner }

  TsardHeaderScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardWhitespaceScanner }

  TsardWhitespaceScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardIdentifierScanner }

  TsardIdentifierScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardNumberScanner }

  TsardNumberScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardBracketScanner }

  TsardBracketScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardTerminalScanner }

  TsardControlScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardOperatorScanner }

  TsardOperatorScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardLineCommentScanner }

  TsardLineCommentScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardBlockCommentScanner }

  TsardBlockCommentScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardSQStringScanner }

  TsardSQStringScanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardDQStringScanner }

  TsardDQStringScanner = class(TsardScanner)
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

procedure TsardControlScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
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

function TsardControlScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in CONTROLS_OPEN_CHARS;
end;

{ TsardBracketScanner }

procedure TsardBracketScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
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

function TsardBracketScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
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
    prcStart := RegisterScanner(TsardStartScanner);
    prcHeader := RegisterScanner(TsardHeaderScanner);
    prcWhiteSpace := RegisterScanner(TsardWhitespaceScanner);
    prcIdentifier := RegisterScanner(TsardIdentifierScanner);
    prcNumber := RegisterScanner(TsardNumberScanner);
    prcBracket := RegisterScanner(TsardBracketScanner);
    prcControl := RegisterScanner(TsardControlScanner);
    prcSQString := RegisterScanner(TsardSQStringScanner);
    prcDQString := RegisterScanner(TsardDQStringScanner);
    prcBlockComment := RegisterScanner(TsardBlockCommentScanner);
    prcLineComment := RegisterScanner(TsardLineCommentScanner);
    prcOperator := RegisterScanner(TsardOperatorScanner);//Register it after comment because comment take /*
  end;
  FOffScanner := prcIdentifier;
end;

{ TsardNumberScanner }

procedure TsardNumberScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
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

function TsardNumberScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in NUMBER_OPEN_CHARS;//need to improve to accept unicode chars
end;

{ TsardOperatorScanner }

procedure TsardOperatorScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
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

function TsardOperatorScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := IsOperator(Text[Column]);
end;

{ TsardIdentifierScanner }

procedure TsardIdentifierScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
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

function TsardIdentifierScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in IDENTIFIER_OPEN_CHARS;//need to improve to accept unicode chars
end;

{ TsardDQStringScanner }

procedure TsardDQStringScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
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

function TsardDQStringScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('"', Text, Column);
end;

{ TsardSQStringScanner }

procedure TsardSQStringScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
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

function TsardSQStringScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('''', Text, Column);
end;

{ TsardBlockCommentScanner }

procedure TsardBlockCommentScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
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

function TsardBlockCommentScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('/*', Text, Column);
end;

{ TsardSLCommentScanner }

procedure TsardLineCommentScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  while (Column <= Length(Text)) and not (Text[Column] in sEOL) do //TODO ignore quoted strings
    Inc(Column);
  //Push(MidStr(Text, c, Column - c)); ignore comment
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
end;

function TsardLineCommentScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('//', Text, Column);
end;

{ TsardHeaderCommentScanner }

procedure TsardHeaderScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
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

function TsardHeaderScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False; //Only when file started, or first, so not accepted
end;

{ TsardNormalCommentScanner }

procedure TsardWhitespaceScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  while (Column <= Length(Text)) and (Text[Column] in sWhitespace) do
    Inc(Column);
  if Column <= Length(Text) then
    ChooseScanner(Text, Column, Line);
end;

function TsardWhitespaceScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in sWhitespace;
end;

{ TsardStartScanner }

procedure TsardStartScanner.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  if LeftStr(Text, Length(sSardAnsiOpen)) = sSardAnsiOpen then
  begin
    //There is a header and it is a Ansi document
    Column := Column + Length(sSardAnsiOpen); //put the column to the first char of attributes of document
    SelectScanner(TsardHeaderScanner);
  end
  else
    ChooseScanner(Text, Column, Line); //nop there is no header... skip to normal
end;

function TsardStartScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False;//Start not accept the scan, it is only when starting scan
end;

end.
