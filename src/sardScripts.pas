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

(*
  Like pascal
    It is case insensitive
    Declareing after the name
    Assigning ":=", compare "=", object child "."
    Not equal operator "<>"

    foo:{
      bar: integer;
      i: integer = 5; //Declare and Assign
      method1:{
        :=i * bar //return value
      }
    }

    foo.bar := 10;

  -----------------------------------------------------
  Like C
    Block { }, no more begin end
    comments //single line and /* multiline */
    Not "!"  or "|"
  -----------------------------------------------------
  Like nothing
    Returning value

   foo:{
     := 10;
   }

   or

   foo:integer{
     := 10;
   }
*)

(*
  With:
    object.{     <-not shure
    }

  Escape char is outside the string there is no escape inside it. sorry for that.
  "test " \" " and"
  it equal to "test " and"
*)

{TODO
  check not add object if there is no operator in curOperator
  do not allow to add empty statment
}

{
  Scope
  Block
  Statment
  Expression
}

interface

uses
  Classes, SysUtils,
  sardClasses, sardObjects;

const
  sEOL = [#0, #13, #10];
  sWhitespace = sEOL + [' ', #9];

  sNumberOpenChars = ['0'..'9'];
  sNumberChars = sNumberOpenChars + ['.', 'x', 'h', 'a'..'f'];

  sColorOpenChars = ['#'];
  sColorChars = sColorOpenChars + ['0'..'9', 'a'..'f'];

  sBracketOpoenChars = ['(', '[', '{'];
  sBracketCloseChars = [')', ']', '}'];
  sBracketChars = sBracketOpoenChars + sBracketCloseChars;

  aControlsOpenChars = [':', '.', ',', ';', '~'] + sBracketChars;
  sControlsChars = aControlsOpenChars + ['='];

  sOperatorOpenChars = ['+', '-', '*', '/', '<', '>', '^', '&', '|', '!', '=', '$', '@']; //<--Stupid idea, need to make it as array

//  IDENTIFIER_OPEN_CHARS = ['A'..'Z', 'a'..'z', '_'];
//  IDENTIFIER_CHARS = IDENTIFIER_OPEN_CHARS + ['0'..'9', '.']; //for : xdebug send tag like xdebug:message

type
  TsrdTokenKinds = (tknWhitespace, tknOperator, tknControl, tknNumber, tknColor, tknString, tknIdentifier, tknComment);

  TsrdState = (stateIdentifier, stateDeclare, stateAssign, stateBlock);
  TsrdStates = set of TsrdState;

  { TsrdScript }

  TsrdScript = class(TsardFeeder)
  private
  protected
  public
    constructor Create; override;
  end;

  { TsrdParserStackItem }

  TsrdParserStackItem = class(TsardObject)
  public
    States: TsrdStates;
    Operation: TopOperator;
    Block: TsrdBlock;
  public
    procedure SetOperator(AOperation: TopOperator);
    procedure SetObject(AObject: TsoObject);
  end;

  { TsrdParserStack }

  TsrdParserStack = class(TsardStack)
  private
    function GetCurrent: TsrdParserStackItem;
  public
    procedure Push(vItem: TsrdParserStackItem);
    function New: TsrdParserStackItem;
    property Current: TsrdParserStackItem read GetCurrent;
  end;

  { TsrdScriptParser }

  TsrdParser = class(TsardParser)
  protected
    FStack: TsrdParserStack;
  public
    constructor Create(AScope: TsrdScope);
    destructor Destroy; override;
    procedure TriggerOpen(vBracket: TsardBracketKind); override;
    procedure TriggerClose(vBracket: TsardBracketKind); override;
    procedure TriggerToken(Token: String; TokenID: Integer); override;
    procedure TriggerControl(AControl: TsardControl); override;
    procedure TriggerOperator(AOperator: TsardObject); override;

    property Stack: TsrdParserStack read FStack;
  end;

  { TsrdStartScanner }

  TsrdStart_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsrdWhitespaceScanner }

  TsrdWhitespace_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsrdIdentifierScanner }

  TsrdIdentifier_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsrdNumberScanner }

  TsrdNumber_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsrdControl_Scanner }

  TsrdControl_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TopOperatorScanner }

  TopOperator_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsrdLineCommentScanner }

  TsrdLineComment_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsrdBlockCommentScanner }

  TsrdBlockComment_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsrdSQStringScanner }

  TsrdSQString_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsrdDQStringScanner }

  TsrdDQString_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

implementation

uses
  StrUtils;

{ TsrdParserStackItem }

procedure TsrdParserStackItem.SetOperator(AOperation: TopOperator);
begin
  if Operation = nil then
    raise EsardException.Create('Operator is already set');
  Operation := AOperation;
end;

procedure TsrdParserStackItem.SetObject(AObject: TsoObject);
begin
  //  if Operation = nil then
  //   raise EsardParserException.Create('Need a operator');
  Block.Statement.Add(Operation, AObject);
  Operation := nil;
end;

{ TsrdParserStack }

function TsrdParserStack.GetCurrent: TsrdParserStackItem;
begin
  Result := (inherited GetCurrent) as TsrdParserStackItem;
end;

procedure TsrdParserStack.Push(vItem: TsrdParserStackItem);
begin
  inherited Push(vItem);
end;

function TsrdParserStack.New: TsrdParserStackItem;
begin
  Result := TsrdParserStackItem.Create;
  Push(Result);
end;

{ TsrdParser }

constructor TsrdParser.Create(AScope: TsrdScope);
begin
  inherited Create;
  FStack := TsrdParserStack.Create;
  if AScope <> nil then
    with Stack.New do
    begin
       Block := AScope;
    end;
end;

destructor TsrdParser.Destroy;
begin
  FreeAndNil(FStack);
  inherited Destroy;
end;

procedure TsrdParser.TriggerOpen(vBracket: TsardBracketKind);
begin
  case vBracket of
    brCurly:
      with TsoScope.Create do
      begin
        Stack.Current.SetObject(This);
        Stack.New;
        Stack.Current.Block := Items;
      end;
    brBracket:
      with TsoBlock.Create do
      begin
        Stack.Current.SetObject(This);
        Stack.New;
        Stack.Current.Block := Items;
      end;
  end;
end;

procedure TsrdParser.TriggerClose(vBracket: TsardBracketKind);
begin
  case vBracket of
    brCurly:
    begin
      if FStack.Current.Operation <> nil then
        raise EsardException.Create('There is opertator but you finished the block');
      Stack.Pop;
    end;
    brBracket:
    begin
      if FStack.Current.Operation <> nil then
        raise EsardException.Create('There is opertator but you finished the block');
      Stack.Pop;
    end;
  end;
end;

procedure TsrdParser.TriggerToken(Token: String; TokenID: Integer);
begin
  case TsrdTokenKinds(TokenID) of
    tknIdentifier:
    begin
      with TsoVariable.Create do
      begin
        Name := Token;
        Stack.Current.SetObject(This);
      end;
    end;
    tknNumber:
    begin
      if pos('.', Token) > 0 then
      begin
        with TsoFloat.Create do
        begin
          Stack.Current.SetObject(This);
          Value := StrToFloat(Token);
        end;
      end
      else
      begin
        with TsoInteger.Create do
        begin
          Stack.Current.SetObject(This);
          Value := StrToInt64(Token);
        end;
      end;
    end;
    tknString:
    begin
      with TsoString.Create do
      begin
        Stack.Current.SetObject(This);
        Value := Token;
      end;
    end;
  end;
end;

procedure TsrdParser.TriggerControl(AControl: TsardControl);
begin
  case AControl of
    ctlSemicolon:
    begin
      if FStack.Current.Operation <> nil then
        raise EsardException.Create('There is opertator but you finished the block');
      Stack.Current.Block.New;
    end;
    ctlAssign:
    begin
//      if FStack.Current.Operation <> nil then
//        raise EsardException.Create('There is opertator but you finished the block');
      Stack.Current.States := Stack.Current.States + [stateAssign];
    end;
  end;
end;

procedure TsrdParser.TriggerOperator(AOperator: TsardObject);
begin
  if Stack.Current.Operation <> nil then
    raise EsardException.Create('Operator already set');
  Stack.Current.Operation := AOperator as TopOperator;
end;

{ TsrdControlScanner }

procedure TsrdControl_Scanner.Scan(const Text: string; var Column: Integer);
var
  b: string;
  l, c: Integer;
begin
  c := Column;
  l := Length(Text);
  while (Column <= l) and (sardEngine.IsControl(Text[Column], False)) do
    Inc(Column);

  b := MidStr(Text, c, Column - c);

  if b = '(' then //TODO need to improve it, my brain is off
    Scanners.Parser.TriggerOpen(brBracket)
  else if b = '[' then
    Scanners.Parser.TriggerOpen(brSquare)
  else if b = '{' then
    Scanners.Parser.TriggerOpen(brCurly)
  else if b = ')' then
    Scanners.Parser.TriggerClose(brBracket)
  else if b = ']' then
    Scanners.Parser.TriggerClose(brSquare)
  else if b = '}' then
    Scanners.Parser.TriggerClose(brCurly)
  else if b = ';' then
    Scanners.Parser.TriggerControl(ctlFinish)
  else if b = ',' then
    Scanners.Parser.TriggerControl(ctlSplit)
  else if b = ':' then
    Scanners.Parser.TriggerControl(ctlDeclare)
  else if b = ':=' then
    Scanners.Parser.TriggerControl(ctlAssign)
  else if b = '~' then
    Scanners.Parser.TriggerControl(ctlPointer);
  Inc(Column);
end;

function TsrdControl_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsControl(Text[Column], True);
end;

{ TsrdScript }

constructor TsrdScript.Create;
begin
  inherited Create;

  with Scanners do
  begin
    RegisterScanner(TsrdStart_Scanner);
    RegisterScanner(TsrdWhitespace_Scanner);
    RegisterScanner(TsrdBlockComment_Scanner);
    RegisterScanner(TsrdLineComment_Scanner);
    RegisterScanner(TsrdNumber_Scanner);
    RegisterScanner(TsrdSQString_Scanner);
    RegisterScanner(TsrdDQString_Scanner);
    RegisterScanner(TsrdControl_Scanner);
    RegisterScanner(TopOperator_Scanner); //Register it after comment because comment take /*
    RegisterScanner(TsrdIdentifier_Scanner);//Last one
  end;
  //FOffScanner := scanIdentifier;
end;

{ TsrdNumberScanner }

procedure TsrdNumber_Scanner.Scan(const Text: string; var Column: Integer);
var
  l, c: Integer;
begin
  c := Column;
  l := Length(Text);
  while (Column <= l) and (sardEngine.IsNumber(Text[Column], False)) do
    Inc(Column);
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), Ord(tknNumber));
end;

function TsrdNumber_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsNumber(Text[Column], True);//need to improve to accept unicode chars
end;

{ TopOperatorScanner }

procedure TopOperator_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
  s: string;
  o: TopOperator;
begin
  c := Column;
  while (Column <= Length(Text)) and (sardEngine.IsOperator(Text[Column])) do //operator is multi char here
    Inc(Column);
  s := MidStr(Text, c, Column - c);

  o := sardEngine.Operators.Find(s);
  if o = nil then
    raise EsardException.Create('Unkown operator: ' + s);
  Scanners.Parser.TriggerOperator(o);
end;

function TopOperator_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsOperator(Text[Column]);
end;

{ TsrdIdentifierScanner }

procedure TsrdIdentifier_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (sardEngine.IsIdentifier(Text[Column], False)) do
    Inc(Column);
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), Ord(tknIdentifier));
end;

function TsrdIdentifier_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsIdentifier(Text[Column], True);//need to improve to accept unicode chars
end;

{ TsrdDQStringScanner }

procedure TsrdDQString_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '"') do //TODO Escape, not now
    Inc(Column);
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), Ord(tknString));
end;

function TsrdDQString_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := CheckText('"', Text, Column);
end;

{ TsrdSQStringScanner }

procedure TsrdSQString_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '''') do //TODO Escape, not now
    Inc(Column);
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), Ord(tknString));
end;

function TsrdSQString_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := CheckText('''', Text, Column);
end;

{ TsrdBlockCommentScanner }

procedure TsrdBlockComment_Scanner.Scan(const Text: string; var Column: Integer);
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

function TsrdBlockComment_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := CheckText('/*', Text, Column);
end;

{ TsrdLineComment_Scanner }

procedure TsrdLineComment_Scanner.Scan(const Text: string; var Column: Integer);
begin
  while (Column <= Length(Text)) and not (Text[Column] in sEOL) do //TODO ignore quoted strings
    Inc(Column);
  //Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c)); ignore comment
end;

function TsrdLineComment_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := CheckText('//', Text, Column);
end;

{ TsrdWhitespace_Scanner }

procedure TsrdWhitespace_Scanner.Scan(const Text: string; var Column: Integer);
begin
  while (Column <= Length(Text)) and (Text[Column] in sWhitespace) do
    Inc(Column);
end;

function TsrdWhitespace_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Text[Column] in sWhitespace;
end;

{ TsrdStart_Scanner }

procedure TsrdStart_Scanner.Scan(const Text: string; var Column: Integer);
begin
end;

function TsrdStart_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := False;//Start not accept the scan, it is only when starting scan
end;

end.
