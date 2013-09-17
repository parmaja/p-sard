unit sardScripts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Sard;

const
  IDENTIFIER_OPEN_CHARS = ['A'..'Z', 'a'..'z', '_'];
  IDENTIFIER_CHARS = IDENTIFIER_OPEN_CHARS + ['0'..'9', '.']; //for : xdebug send tag like xdebug:message
  NUMBER_OPEN_CHARS = ['0'..'9'];
  NUMBER_CHARS = NUMBER_OPEN_CHARS + ['.','E'];
  OPERATOR_OPEN_CHARS = ['+', '-', '*', '/', '^', '&'];
  OPERATOR_CHARS = OPERATOR_OPEN_CHARS {+ ['']};
  SYMBOL_CHARS = ['$', '#', '@', '!', '\'];
  BRACKET_OPEN_CHARS = ['(', '[', '{'];
  BRACKET_CLOSE_CHARS= [')', ']', '}'];
  BRACKET_CHARS = BRACKET_OPEN_CHARS + BRACKET_CLOSE_CHARS;
  TERMINAL_CHARS = [';', ','];
  sSardAnsiOpen = '{?sard '; //with the space

type
  TsardTokenKinds = (tknOperator, tknTerminal, tknBracket, tknIdentifier, tknString, tknNumber, tknComment);

{ TsardScanner }

  TsardScript = class(TsardCustomScanner)
  private
    //       <--------- i don't know why i save it in variables, so it is temporary solution
    prcStart: Integer;
    prcHeader: Integer;
    prcWhiteSpace: Integer;
    prcIdentifier: Integer;
    prcNumber: Integer;
    prcBracket: Integer;
    prcOperator: Integer;
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

  { TsardStartProcess }

  TsardStartProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardHeaderProcess }

  TsardHeaderProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardWhitespaceProcess }

  TsardWhitespaceProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardIdentifierProcess }

  TsardIdentifierProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardNumberProcess }

  TsardNumberProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardBracketProcess }

  TsardBracketProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardTerminalProcess }

  TsardTerminalProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardOperatorProcess }

  TsardOperatorProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardLineCommentProcess }

  TsardLineCommentProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardBlockCommentProcess }

  TsardBlockCommentProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardSQStringProcess }

  TsardSQStringProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardDQStringProcess }

  TsardDQStringProcess = class(TsardProcess)
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

{ TsardTerminalProcess }

procedure TsardTerminalProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  b: AnsiChar;
begin
  b := Text[Column];
  if b = ';' then //TODO need to improve it
    Terminate
  else if b = ',' then
    Terminate
end;

function TsardTerminalProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in TERMINAL_CHARS;
end;

{ TsardBracketProcess }

procedure TsardBracketProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
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

function TsardBracketProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
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

  with Processes do
  begin
    prcStart := RegisterProcess(TsardStartProcess);
    prcHeader := RegisterProcess(TsardHeaderProcess);
    prcWhiteSpace := RegisterProcess(TsardWhitespaceProcess);
    prcIdentifier := RegisterProcess(TsardIdentifierProcess);
    prcNumber := RegisterProcess(TsardNumberProcess);
    prcBracket := RegisterProcess(TsardBracketProcess);
    prcSQString := RegisterProcess(TsardSQStringProcess);
    prcDQString := RegisterProcess(TsardDQStringProcess);
    prcBlockComment := RegisterProcess(TsardBlockCommentProcess);
    prcLineComment := RegisterProcess(TsardLineCommentProcess);
    prcOperator := RegisterProcess(TsardOperatorProcess);//Register it after comment because comment take /*
  end;
  FOffProcess := prcIdentifier;
end;

{ TsardNumberProcess }

procedure TsardNumberProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  l, c: Integer;
begin
  c := Column;
  l := Length(Text);
  while (Column <= l) and (Text[Column] in NUMBER_CHARS) do
    Inc(Column);
  Push(MidStr(Text, c, Column - c), Ord(tknNumber));
  if Column <= Length(Text) then
    ChooseProcess(Text, Column, Line);
end;

function TsardNumberProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in NUMBER_OPEN_CHARS;//need to improve to accept unicode chars
end;

{ TsardOperatorProcess }

procedure TsardOperatorProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (IsOperator(Text[Column])) do //operator is multi char here
    Inc(Column);
  Push(MidStr(Text, c, Column - c), Ord(tknOperator));
  if Column <= Length(Text) then
    ChooseProcess(Text, Column, Line);
end;

function TsardOperatorProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := IsOperator(Text[Column]);
end;

{ TsardIdentifierProcess }

procedure TsardIdentifierProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (Text[Column] in IDENTIFIER_CHARS) do
    Inc(Column);
  Push(MidStr(Text, c, Column - c), Ord(tknIdentifier));
  if Column <= Length(Text) then
    ChooseProcess(Text, Column, Line);
end;

function TsardIdentifierProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in IDENTIFIER_OPEN_CHARS;//need to improve to accept unicode chars
end;

{ TsardDQStringProcess }

procedure TsardDQStringProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '"') do //TODO Escape, not now
    Inc(Column);
  Push(MidStr(Text, c, Column - c), Ord(tknString));
  if Column <= Length(Text) then
    ChooseProcess(Text, Column, Line);
end;

function TsardDQStringProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('"', Text, Column);
end;

{ TsardSQStringProcess }

procedure TsardSQStringProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '''') do //TODO Escape, not now
    Inc(Column);
  Push(MidStr(Text, c, Column - c), Ord(tknString));
  if Column <= Length(Text) then
    ChooseProcess(Text, Column, Line);
end;

function TsardSQStringProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('''', Text, Column);
end;

{ TsardBlockCommentProcess }

procedure TsardBlockCommentProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
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
    ChooseProcess(Text, Column, Line);
end;

function TsardBlockCommentProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('/*', Text, Column);
end;

{ TsardSLCommentProcess }

procedure TsardLineCommentProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  while (Column <= Length(Text)) and not (Text[Column] in sEOL) do //TODO ignore quoted strings
    Inc(Column);
  //Push(MidStr(Text, c, Column - c)); ignore comment
  if Column <= Length(Text) then
    ChooseProcess(Text, Column, Line);
end;

function TsardLineCommentProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('//', Text, Column);
end;

{ TsardHeaderCommentProcess }

procedure TsardHeaderProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (Text[Column] <> '}') do
    Inc(Column);
  Push(MidStr(Text, c, Column - c), 0);
  if Column <= Length(Text) then
    ChooseProcess(Text, Column, Line);
end;

function TsardHeaderProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False; //Only when file started, or first, so not accepted
end;

{ TsardNormalCommentProcess }

procedure TsardWhitespaceProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  while (Column <= Length(Text)) and (Text[Column] in sWhitespace) do
    Inc(Column);
  if Column <= Length(Text) then
    ChooseProcess(Text, Column, Line);
end;

function TsardWhitespaceProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in sWhitespace;
end;

{ TsardStartProcess }

procedure TsardStartProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  if LeftStr(Text, Length(sSardAnsiOpen)) = sSardAnsiOpen then
  begin
    //There is a header and it is a Ansi document
    Column := Column + Length(sSardAnsiOpen); //put the column to the first char of attributes of document
    SelectProcess(TsardHeaderProcess);
  end
  else
    ChooseProcess(Text, Column, Line); //nop there is no header... skip to normal
end;

function TsardStartProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False;//Start not accept the scan, it is only when starting scan
end;

end.
