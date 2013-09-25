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
  TsardTokenKinds = (tknWhitespace, tknOperator, tknControl, tknNumber, tknColor, tknString, tknIdentifier, tknComment);

  TsardState = (stateIdentifier, stateDeclare, stateAssign, stateBlock);
  TsardStates = set of TsardState;

  { TsardScript }

  TsardScript = class(TsardFeeder)
  private
  protected
  public
    constructor Create; override;
  end;

  { TsardParserStackItem }

  TsardParserStackItem = class(TsardObject)
  public
    States: TsardStates;
    Operation: TsrdoOperator;
    Block: TsrdoBlock;
  public
    procedure SetOperator(AOperation: TsrdoOperator);
    procedure SetObject(AObject: TsrdoObject);
  end;

  { TParserStack }

  { TsardParserStack }

  TsardParserStack = class(TsardStack)
  private
    function GetCurrent: TsardParserStackItem;
  public
    procedure Push(vItem: TsardParserStackItem);
    function New: TsardParserStackItem;
    property Current: TsardParserStackItem read GetCurrent;
  end;

  { TsardScriptParser }

  TsardScanParser = class(TsardParser)
  protected
    FStack: TsardParserStack;
  public
    constructor Create(ABlock: TsrdoBlock);
    destructor Destroy; override;
    procedure TriggerOpen(vBracket: TsardBracketKind); override;
    procedure TriggerClose(vBracket: TsardBracketKind); override;
    procedure TriggerToken(Token: String; TokenID: Integer); override;
    procedure TriggerControl(AControl: TsardControl); override;
    procedure TriggerOperator(AOperator: TsardObject); override;

    property Stack: TsardParserStack read FStack;
  end;

  { TsardStartScanner }

  TsardStart_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsardWhitespaceScanner }

  TsardWhitespace_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsardIdentifierScanner }

  TsardIdentifier_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsardNumberScanner }

  TsardNumber_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsardControl_Scanner }

  TsardControl_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsardOperatorScanner }

  TsardOperator_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsardLineCommentScanner }

  TsardLineComment_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsardBlockCommentScanner }

  TsrdoBlockComment_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsardSQStringScanner }

  TsardSQString_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsardDQStringScanner }

  TsardDQString_Scanner = class(TsardScanner)
  protected
    procedure Scan(const Text: string; var Column: Integer);  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

implementation

uses
  StrUtils;

{ TsardParserStackItem }

procedure TsardParserStackItem.SetOperator(AOperation: TsrdoOperator);
begin
  if Operation = nil then
    raise EsardException.Create('Operator is already set');
  Operation := AOperation;
end;

procedure TsardParserStackItem.SetObject(AObject: TsrdoObject);
begin
  //  if Operation = nil then
  //   raise EsardParserException.Create('Need a operator');
  Block.Statement.Add(Operation, AObject);
  Operation := nil;
end;

{ TsardParserStack }

function TsardParserStack.GetCurrent: TsardParserStackItem;
begin
  Result := (inherited GetCurrent) as TsardParserStackItem;
end;

procedure TsardParserStack.Push(vItem: TsardParserStackItem);
begin
  inherited Push(vItem);
end;

function TsardParserStack.New: TsardParserStackItem;
begin
  Result := TsardParserStackItem.Create;
  Push(Result);
end;

{ TsardScanParser }

constructor TsardScanParser.Create(ABlock: TsrdoBlock);
begin
  inherited Create;
  FStack := TsardParserStack.Create;
  if ABlock <> nil then
    with Stack.New do
    begin
       Block := ABlock;
    end;
end;

destructor TsardScanParser.Destroy;
begin
  FreeAndNil(FStack);
  inherited Destroy;
end;

procedure TsardScanParser.TriggerOpen(vBracket: TsardBracketKind);
begin
  case vBracket of
    brCurly:
      with TsrdoBranch.Create do
      begin
        Stack.Current.SetObject(This);
        Stack.New;
        Stack.Current.Block := Block;
      end;
  end;
end;

procedure TsardScanParser.TriggerClose(vBracket: TsardBracketKind);
begin
  case vBracket of
    brCurly:
    begin
      if Stack.IsEmpty then
        raise EsardException.Create('There is no opened block!');
      if FStack.Current.Operation <> nil then
        raise EsardException.Create('There is opertator but you finished the block');
      Stack.Pop;
    end;
  end;
end;

procedure TsardScanParser.TriggerToken(Token: String; TokenID: Integer);
begin
  case TsardTokenKinds(TokenID) of
    tknIdentifier:
    begin
      with TsrdoVariable.Create do
      begin
        Name := Token;
        Stack.Current.SetObject(This);
      end;
    end;
    tknNumber:
    begin
      if pos('.', Token) > 0 then
      begin
        with TsrdoFloat.Create do
        begin
          Stack.Current.SetObject(This);
          Value := StrToFloat(Token);
        end;
      end
      else
      begin
        with TsrdoInteger.Create do
        begin
          Stack.Current.SetObject(This);
          Value := StrToInt64(Token);
        end;
      end;
    end;
    tknString:
    begin
      with TsrdoString.Create do
      begin
        Stack.Current.SetObject(This);
        Value := Token;
      end;
    end;
  end;
end;

procedure TsardScanParser.TriggerControl(AControl: TsardControl);
begin
  case AControl of
    ctlSemicolon: Stack.Current.Block.New;
    ctlAssign: Stack.Current.States := Stack.Current.States + [stateAssign];
  end;
end;

procedure TsardScanParser.TriggerOperator(AOperator: TsardObject);
begin
  if Stack.Current.Operation <> nil then
    raise EsardException.Create('Operator already set');
  Stack.Current.Operation := AOperator as TsrdoOperator;
end;

{ TsardControlScanner }

procedure TsardControl_Scanner.Scan(const Text: string; var Column: Integer);
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

function TsardControl_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsControl(Text[Column], True);
end;

{ TsardScript }

constructor TsardScript.Create;
begin
  inherited Create;

  with Scanners do
  begin
    RegisterScanner(TsardStart_Scanner);
    RegisterScanner(TsardWhitespace_Scanner);
    RegisterScanner(TsrdoBlockComment_Scanner);
    RegisterScanner(TsardLineComment_Scanner);
    RegisterScanner(TsardNumber_Scanner);
    RegisterScanner(TsardSQString_Scanner);
    RegisterScanner(TsardDQString_Scanner);
    RegisterScanner(TsardControl_Scanner);
    RegisterScanner(TsardOperator_Scanner); //Register it after comment because comment take /*
    RegisterScanner(TsardIdentifier_Scanner);//Last one
  end;
  //FOffScanner := scanIdentifier;
end;

{ TsardNumberScanner }

procedure TsardNumber_Scanner.Scan(const Text: string; var Column: Integer);
var
  l, c: Integer;
begin
  c := Column;
  l := Length(Text);
  while (Column <= l) and (sardEngine.IsNumber(Text[Column], False)) do
    Inc(Column);
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), Ord(tknNumber));
end;

function TsardNumber_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsNumber(Text[Column], True);//need to improve to accept unicode chars
end;

{ TsardOperatorScanner }

procedure TsardOperator_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
  s: string;
  o: TsrdoOperator;
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

function TsardOperator_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsOperator(Text[Column]);
end;

{ TsardIdentifierScanner }

procedure TsardIdentifier_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (sardEngine.IsIdentifier(Text[Column], False)) do
    Inc(Column);
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), Ord(tknIdentifier));
end;

function TsardIdentifier_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsIdentifier(Text[Column], True);//need to improve to accept unicode chars
end;

{ TsardDQStringScanner }

procedure TsardDQString_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '"') do //TODO Escape, not now
    Inc(Column);
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), Ord(tknString));
end;

function TsardDQString_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := CheckText('"', Text, Column);
end;

{ TsardSQStringScanner }

procedure TsardSQString_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '''') do //TODO Escape, not now
    Inc(Column);
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), Ord(tknString));
end;

function TsardSQString_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := CheckText('''', Text, Column);
end;

{ TsardBlockCommentScanner }

procedure TsrdoBlockComment_Scanner.Scan(const Text: string; var Column: Integer);
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

function TsrdoBlockComment_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := CheckText('/*', Text, Column);
end;

{ TsardLineComment_Scanner }

procedure TsardLineComment_Scanner.Scan(const Text: string; var Column: Integer);
begin
  while (Column <= Length(Text)) and not (Text[Column] in sEOL) do //TODO ignore quoted strings
    Inc(Column);
  //Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c)); ignore comment
end;

function TsardLineComment_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := CheckText('//', Text, Column);
end;

{ TsardWhitespace_Scanner }

procedure TsardWhitespace_Scanner.Scan(const Text: string; var Column: Integer);
begin
  while (Column <= Length(Text)) and (Text[Column] in sWhitespace) do
    Inc(Column);
end;

function TsardWhitespace_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Text[Column] in sWhitespace;
end;

{ TsardStart_Scanner }

procedure TsardStart_Scanner.Scan(const Text: string; var Column: Integer);
begin
end;

function TsardStart_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := False;//Start not accept the scan, it is only when starting scan
end;

end.
