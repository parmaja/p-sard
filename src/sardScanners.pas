unit sardScanners;
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
    Dot as Identifier separator "."
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

   Description: new comment but saved with the objects.
   {* This object not usfule dont use it *}
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
  Outch
  how to scan, minus here?
  x := -10;

  check not add object if there is no operator in curOperator
  do not allow to add empty statment
}

{
  Scope
  Block
  Statment
  Instruction
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

  aControlsOpenChars = [':', ',', ';', '~'] + sBracketChars;
  sControlsChars = aControlsOpenChars + ['='];

  sOperatorOpenChars = ['+', '-', '*', '/', '<', '>', '^', '&', '|', '!', '=', '$', '@']; //<--Stupid idea, need to make it as array

  sIdentifierSeparator = '.';

type
  TsrdState = (stateDeclare, stateAssign, stateBlock, stateIdentifier, stateNumber, stateString, stateComment, stateOperator);
  TsrdStates = set of TsrdState;

  TsrdFeeder = class(TsardFeeder)
  end;

  { TsrdScanners }

  TsrdScanners = class(TsardScanners)
  private
  protected
    procedure Created; override;
  public
  end;

  { TsrdParserStackItem }

  TsrdParserStackItem = class(TsardObject)
  private
    FStates: TsrdStates;
  public
    Identifier: string;
    Operation: TopOperator;
    Block: TsrdBlock;
  public
    procedure SetOperator(AOperation: TopOperator);
    procedure SetObject(AObject: TsoObject);
    procedure SetState(AStates: TsrdStates);
    procedure Reset;
    procedure NewStatement;
    property States: TsrdStates read FStates;
  end;

  { TsrdParserStack }

  TsrdParserStack = class(TsardStack)
  private
    function GetCurrent: TsrdParserStackItem;
  public
    procedure Push(vItem: TsrdParserStackItem);
    function Push: TsrdParserStackItem;
    property Current: TsrdParserStackItem read GetCurrent;
  end;

  { TsrdFeederParser }

  TsrdParser = class(TsardParser)
  protected
    FStack: TsrdParserStack;
  public
    constructor Create(ABlock: TsrdBlock);
    destructor Destroy; override;
    procedure TriggerOpen(vBracket: TsardBracketKind); override;
    procedure TriggerClose(vBracket: TsardBracketKind); override;
    procedure TriggerIdentifier(Token: String); override;
    procedure TriggerNumber(Token: String); override;
    procedure TriggerString(Token: String); override;
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

procedure TsrdParserStackItem.SetState(AStates: TsrdStates);
begin
  FStates := FStates + AStates;
end;

procedure TsrdParserStackItem.Reset;
begin
  FStates := [];
  Identifier := '';
  Operation := nil;
end;

procedure TsrdParserStackItem.NewStatement;
begin
  Reset;
  Block.Add;
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

function TsrdParserStack.Push: TsrdParserStackItem;
begin
  Result := TsrdParserStackItem.Create;
  Push(Result);
end;

{ TsrdParser }

constructor TsrdParser.Create(ABlock: TsrdBlock);
begin
  inherited Create;
  FStack := TsrdParserStack.Create;
  if ABlock <> nil then
    with Stack.Push do
    begin
       Block := ABlock;
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
      if Stack.Current.States = [stateIdentifier] then
      begin
        with TsoClass.Create do
        begin
          Name := Stack.Current.Identifier;
          Stack.Current.SetObject(This);
          Stack.Push;
          Stack.Current.Block := Items;
        end;
      end
      else
        with TsoBranch.Create do
        begin
          Stack.Current.SetObject(This);
          Stack.Push;
          Stack.Current.Block := Items;
        end;
    brBracket:
      with TsoBlock.Create do
      begin
        Stack.Current.SetObject(This);
        Stack.Push;
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
{      if FStack.Current.Block.Count = 0 then
        raise EsardException.Create('Hmmm why empty block!');}//Warning not Error
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

procedure TsrdParser.TriggerIdentifier(Token: String);
begin
  Stack.Current.Identifier := Token;
  with TsoVariable.Create do
  begin
    Name := Token;
    Stack.Current.SetObject(This);
    Stack.Current.SetState([stateIdentifier]);
  end;
end;

procedure TsrdParser.TriggerNumber(Token: String);
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
  Stack.Current.SetState([stateNumber]);
end;

procedure TsrdParser.TriggerString(Token: String);
begin
  with TsoString.Create do
  begin
    Stack.Current.SetObject(This);
    Value := Token;
  end;
  Stack.Current.SetState([stateString]);
end;

procedure TsrdParser.TriggerControl(AControl: TsardControl);
begin
  case AControl of
    ctlSemicolon:
    begin
      if FStack.Current.Operation <> nil then
        raise EsardException.Create('There is opertator but you finished the block');
      Stack.Current.NewStatement;
    end;
    ctlAssign:
    begin
      if (Stack.Current.States = []) or (Stack.Current.States = [stateIdentifier]) then
      begin
        Stack.Current.SetState([stateAssign]);
      end
      else
        raise EsardException.Create('You can not use assignment here!');
    end;
    ctlDeclare:
      begin
        if (Stack.Current.States = [stateIdentifier]) then
        begin
          Stack.Current.SetState([stateDeclare]);
        end
        else
          raise EsardException.Create('You can not use assignment here!');
      end
    else
      raise EsardException.Create('Not implemented yet, sorry :(');
  end;
end;

procedure TsrdParser.TriggerOperator(AOperator: TsardObject);
begin
  if Stack.Current.Operation <> nil then
    raise EsardException.Create('Operator already set');
  Stack.Current.Operation := AOperator as TopOperator;
  Stack.Current.SetState([stateOperator]);
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
end;

function TsrdControl_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsControl(Text[Column], True);
end;

{ TsrdFeeder }

procedure TsrdScanners.Created;
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
  Scanners.Parser.TriggerNumber(MidStr(Text, c, Column - c));
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
  Scanners.Parser.TriggerIdentifier(MidStr(Text, c, Column - c));
end;

function TsrdIdentifier_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsIdentifier(Text[Column], True);
end;

{ TsrdDQStringScanner }

procedure TsrdDQString_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  Inc(Column);//First char
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '"') do //TODO Escape, not now
    Inc(Column);
  Scanners.Parser.TriggerString(MidStr(Text, c, Column - c));
  Inc(Column);
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
  Inc(Column);
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '''') do //TODO Escape, not now
    Inc(Column);
  Scanners.Parser.TriggerString(MidStr(Text, c, Column - c));
  Inc(Column);
end;

function TsrdSQString_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := CheckText('''', Text, Column);
end;

{ TsrdBlockCommentScanner }

procedure TsrdBlockComment_Scanner.Scan(const Text: string; var Column: Integer);
begin
  Inc(Column, 2);//2 chars
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
  Inc(Column, 2);//2 chars
  while (Column <= Length(Text)) and not (Text[Column] in sEOL) do //TODO ignore quoted strings
    Inc(Column);
  //Scanners.Parser.TriggerIdentifier(MidStr(Text, c, Column - c)); ignore comment
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
