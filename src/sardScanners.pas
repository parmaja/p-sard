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
  TsrdFlag = (flagBlock, flagDeclare, flagAssign, flagIdentifier, flagNumber, flagString, flagOperator, flagComment);
  TsrdFlags = set of TsrdFlag;

  { TsrdFeeder }

  TsrdFeeder = class(TsardFeeder)
  protected
    procedure DoStart; override;
    procedure DoStop; override;
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
    FFlags: TsrdFlags;
  public
    TokenType: TsrdType; //Type of Identifier
    AnOperator: TopOperator;
    Token: string;
    Block: TsrdBlock;
  public
    procedure SetIdentifier(AIdentifier: string; ATokenType: TsrdType);
    procedure SetOperator(AOperator: TopOperator);
    procedure SetObject(AObject: TsoObject);
    procedure SetFlags(AFlags: TsrdFlags);
    procedure Reset;
    procedure NewStatement;
    property Flags: TsrdFlags read FFlags;
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
    procedure SetAsNumber;
    procedure SetAsString;
    procedure SetAsIdentifier;
    procedure AddObject;
    procedure TriggerOpen(vBracket: TsardBracketKind); override;
    procedure TriggerClose(vBracket: TsardBracketKind); override;
    procedure TriggerToken(AToken: String; AType: TsrdType); override;
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

{ TsrdFeeder }

procedure TsrdFeeder.DoStart;
begin
  inherited;
  Scanners.Parser.TriggerControl(ctlStart);
end;

procedure TsrdFeeder.DoStop;
begin
  inherited;
  Scanners.Parser.TriggerControl(ctlStop);
end;

{ TsrdParserStackItem }

procedure TsrdParserStackItem.SetIdentifier(AIdentifier: string; ATokenType: TsrdType);
begin
  Token := AIdentifier;
  TokenType := ATokenType;
end;

procedure TsrdParserStackItem.SetOperator(AOperator: TopOperator);
begin
  if AnOperator = nil then
    raise EsardException.Create('Operator is already set');
  AnOperator := AOperator;
end;

procedure TsrdParserStackItem.SetObject(AObject: TsoObject);
begin
  //  if Operation = nil then
  //   raise EsardParserException.Create('Need a operator');
  Block.Statement.Add(AnOperator, AObject);
//  AnOperator := nil;
end;

procedure TsrdParserStackItem.SetFlags(AFlags: TsrdFlags);
begin
  FFlags := FFlags + AFlags;
end;

procedure TsrdParserStackItem.Reset;
begin
  FFlags := [];
  Token := '';
  AnOperator := nil;
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

procedure TsrdParser.SetAsNumber;
begin
  with Stack.Current do
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
    Stack.Current.SetFlags([flagNumber]);
  end;
end;

procedure TsrdParser.SetAsString;
begin
  with Stack.Current do
  begin
    with TsoString.Create do
    begin
      Stack.Current.SetObject(This);
      Value := Token;
    end;
    Stack.Current.SetFlags([flagString]);
  end;
end;

procedure TsrdParser.SetAsIdentifier;
begin

end;

procedure TsrdParser.AddObject;
begin
  if (Stack.Current.Block.Count > 0) and (Stack.Current.AnOperator = nil) then
    raise EsardException.Create('You cant add object with out operator!');

  if (Stack.Current.Token = '') then
    raise EsardException.Create('You cant add empty!');

  case Stack.Current.TokenType of
    tpString: SetAsString;
    tpNumber: SetAsNumber;
    tpIdentifier: SetAsIdentifier;
  end;
  Stack.Current.AnOperator := nil;
  Stack.Current.Token := '';
  Stack.Current.TokenType := tpNone;
end;

procedure TsrdParser.TriggerOpen(vBracket: TsardBracketKind);
begin
  case vBracket of
    brCurly:
{      if Stack.Current.Flags = [flagDeclare, flagIdentifier] then //nop
      begin
        with TsoDeclare.Create do
        begin
          Name := Stack.Current.Token;
          Stack.Current.SetObject(This);
          Stack.Push;
          Stack.Current.Block := Items;
        end;
      end
      else}
        with TsoSection.Create do
        begin
          Stack.Current.SetObject(This);
          Stack.Push;
          Stack.Current.Block := Items;
        end;
    brBracket:
      with TsoDescend.Create do
      begin
        Stack.Current.SetObject(This);
        Stack.Push;
        Stack.Current.Block := Items;
      end;
  end;
end;

procedure TsrdParser.TriggerClose(vBracket: TsardBracketKind);
begin
  AddObject;
  case vBracket of
    brCurly:
    begin
      TriggerControl(ctlSemicolon);
      if FStack.Current.AnOperator <> nil then
        raise EsardException.Create('There is opertator but you finished the block');
{      if FStack.Current.Block.Count = 0 then
        raise EsardException.Create('Hmmm why empty block!');}//Warning not Error
      Stack.Pop;
    end;
    brBracket:
    begin
      TriggerControl(ctlSemicolon);
      if FStack.Current.AnOperator <> nil then
        raise EsardException.Create('There is opertator but you finished the block');
      Stack.Pop;
    end;
  end;
end;

procedure TsrdParser.TriggerToken(AToken: String; AType: TsrdType);
begin
  if Stack.Current.Token <> '' then
    raise EsardException.Create('Already we have object "' + Stack.Current.Token + '"');
  Stack.Current.Token := AToken;
  Stack.Current.TokenType := AType;
{
  with TsoAssign.Create do
  begin
    Name := Token;
    Stack.Current.SetObject(This);
    Stack.Current.SetFlags([stateIdentifier]);
  end;}
end;

procedure TsrdParser.TriggerControl(AControl: TsardControl);
begin
  case AControl of
    ctlStart:
    begin
    end;
    ctlStop:
    begin
      if FStack.Current.AnOperator <> nil then
        raise EsardException.Create('There is opertator but you finished the block');
      AddObject;
    end;
    ctlSemicolon:
    begin
      if FStack.Current.AnOperator <> nil then
        raise EsardException.Create('There is opertator but you finished the block');
      AddObject;
      Stack.Current.NewStatement;
    end;
    ctlAssign:
    begin
      if (Stack.Current.Flags = []) or (Stack.Current.Flags = [flagIdentifier]) then
      begin
        Stack.Current.SetFlags([flagAssign]);
      end
      else
        raise EsardException.Create('You can not use assignment here!');
    end;
    ctlDeclare:
      begin
        if (Stack.Current.Flags = [flagIdentifier]) then
        begin
          Stack.Current.SetFlags([flagDeclare]);
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
  AddObject;
  if Stack.Current.AnOperator <> nil then
    raise EsardException.Create('Operator already set');
  Stack.Current.AnOperator := AOperator as TopOperator;
  Stack.Current.SetFlags([flagOperator]);
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
    Scanners.Parser.TriggerControl(ctlSemicolon)
  else if b = ',' then
    Scanners.Parser.TriggerControl(ctlComma)
  else if b = ':' then
    Scanners.Parser.TriggerControl(ctlDeclare)
  else if b = ':=' then
    Scanners.Parser.TriggerControl(ctlAssign);
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
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), tpNumber);
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
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), tpIdentifier);
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
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), tpString);
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
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), tpString);
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
