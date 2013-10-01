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
  TsrdFlag = (flagBlock, flagInstance, flagDeclare, flagAssign, flagIdentifier, flagNumber, flagString, flagObject, flagOperator, flagComment);
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

  TsrdParser = class;

  TsrdObjectStyle = (tsConst, tsInstance, tsDeclare, tsAssign, tsBlock);

  { TsrdParserBuffer }

  TsrdParserBuffer = class(TsardObject)
  public
    Token: string;
    TokenType: TsrdType;

    TokenOperator: TopOperator;
    TokenStyle: TsrdObjectStyle;
    TokenObject: TsoObject;
    procedure Convert;
  end;

  { TsrdParserStackItem }

  TsrdParserStackItem = class(TsardObject)
  private
    FFlags: TsrdFlags;
  protected
    Parser: TsrdParser;
    Buffer: TsrdParserBuffer;
    Block: TsrdBlock;
    procedure CheckBuffer;
  public
    procedure ConvertNumber;
    procedure ConvertString;
    procedure ConvertComment;
    procedure ConvertObject;

    procedure SetOperator(AOperator: TopOperator);
    procedure SetStyle(AStyle: TsrdObjectStyle);
    procedure SetIdentifier(AIdentifier: string; ATokenType: TsrdType);
    procedure SetObject(AObject: TsoObject);
    procedure SetFlags(AFlags: TsrdFlags);
    procedure Flush;
    procedure NewStatement;
    constructor Create(AParser: TsrdParser);
    destructor Destroy; override;
    property Flags: TsrdFlags read FFlags;
  end;

  { TsrdParserStack }

  TsrdParserStack = class(TsardStack)
  private
    function GetCurrent: TsrdParserStackItem;
  protected
    Parser: TsrdParser;
  public
    procedure Push(vItem: TsrdParserStackItem);
    function Push: TsrdParserStackItem;
    constructor Create(AParser: TsrdParser);
    property Current: TsrdParserStackItem read GetCurrent;
  end;

  { TsrdFeederParser }

  TsrdParser = class(TsardParser)
  protected
    FStack: TsrdParserStack;
    FData: TrunData;
  public
    procedure Start; override;
    procedure Stop; override;
    constructor Create(AData: TrunData; ABlock: TsrdBlock);
    destructor Destroy; override;

    procedure TriggerToken(AToken: String; AType: TsrdType); override;
    procedure TriggerControl(AControl: TsardControl); override;
    procedure TriggerOperator(AOperator: TsardObject); override;
    procedure Flush;
    property Stack: TsrdParserStack read FStack;
    property Data: TrunData read FData;
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

  { TsrdComment_Scanner }
  { it is stored with compiled objects}

  TsrdComment_Scanner = class(TsardScanner)
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

{ TsrdComment_Scanner }

procedure TsrdComment_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  Inc(Column, 2);//2 chars
  while (Column <= Length(Text)) do
  begin
    if (ScanCompare('*}', Text, Column)) then
    begin
      Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), tpComment);
      Inc(Column, 2);//2 chars
      break;
    end;
    Inc(Column);
  end;
end;

function TsrdComment_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanCompare('{*', Text, Column);
end;

{ TsrdParserBuffer }

procedure TsrdParserBuffer.Convert;
begin
end;

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
  CheckBuffer;
  if Buffer.Token <> '' then
    RaiseError('Identifier is already set');
  Buffer.Token := AIdentifier;
  Buffer.TokenType := ATokenType;
  SetFlags([flagIdentifier]);
end;

procedure TsrdParserStackItem.SetOperator(AOperator: TopOperator);
begin
  CheckBuffer;
  if Buffer.TokenOperator <> nil then
    RaiseError('Operator is already set');
  Buffer.TokenOperator := AOperator;
end;

procedure TsrdParserStackItem.SetStyle(AStyle: TsrdObjectStyle);
begin
  CheckBuffer;
  Buffer.TokenStyle := AStyle;
end;

procedure TsrdParserStackItem.SetObject(AObject: TsoObject);
begin
  CheckBuffer;
  if Buffer.TokenObject <> nil then
    RaiseError('Object is already set');
  Buffer.TokenObject := AObject;
end;

procedure TsrdParserStackItem.SetFlags(AFlags: TsrdFlags);
begin
  CheckBuffer;
  FFlags := FFlags + AFlags;
end;

procedure TsrdParserStackItem.Flush;
begin
  if Buffer <> nil then
    with Buffer do
    begin
      if (flagObject in Flags) and (TokenOperator = nil) then
        RaiseError('You cant add object without operator!');

      Convert;

      with Buffer do
        case TokenType of
          tpString: ConvertString;
          tpNumber: ConvertNumber;
          tpComment: ConvertComment;
          else
            ConvertObject;
        end;
  {    if (Block.Count > 0) and (TokenOperator = nil) then }

      if TokenObject = nil then
        RaiseError('Object is nil!');
      Block.Statement.Add(TokenOperator, TokenObject);
    end;
  FreeAndNil(Buffer);
end;

procedure TsrdParserStackItem.NewStatement;
begin
  Block.Add;
  FFlags := [];
end;

constructor TsrdParserStackItem.Create(AParser: TsrdParser);
begin
  inherited Create;
  Parser := AParser;
end;

destructor TsrdParserStackItem.Destroy;
begin
  inherited;
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
  Result := TsrdParserStackItem.Create(Parser);
  Push(Result);
end;

constructor TsrdParserStack.Create(AParser: TsrdParser);
begin
  inherited Create;
  Parser := AParser;
end;

{ TsrdParser }

procedure TsrdParser.Start;
begin
  if Data = nil then
    RaiseError('You must define Data object');
end;

procedure TsrdParser.Stop;
begin

end;

constructor TsrdParser.Create(AData: TrunData; ABlock: TsrdBlock);
begin
  inherited Create;
  FData := AData;
  FStack := TsrdParserStack.Create(Self);
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

procedure TsrdParserStackItem.CheckBuffer;
begin
  if Buffer = nil then
    Buffer := TsrdParserBuffer.Create;
end;

procedure TsrdParserStackItem.ConvertString;
begin
  with Buffer do
  begin
    if Token ='' then
      RaiseError('Nothing to convert to string object');
    with TsoString.Create do
    begin
      Value := Token;
      TokenObject := This;
    end;
    Token := '';
  end;
  SetFlags([flagObject, flagString]);
end;

procedure TsrdParserStackItem.ConvertComment;
begin
  with Buffer do
  begin
    with TsoComment.Create do
    begin
      Value := Token;
      TokenObject := This;
    end;
    Token := '';
  end;
  SetFlags([flagObject, flagComment]);
end;

procedure TsrdParserStackItem.ConvertNumber;
begin
  with Buffer do
  begin
    if Token ='' then
      RaiseError('Nothing to convert to number object');

    if pos('.', Token) > 0 then
    begin
      with TsoFloat.Create do
      begin
        Value := StrToFloat(Token);
        TokenObject := This;
      end;
    end
    else
    begin
      with TsoInteger.Create do
      begin
        Value := StrToInt64(Token);
        TokenObject := This;
      end;
    end;
    Token := '';
  end;
  SetFlags([flagObject, flagNumber]);
end;

procedure TsrdParserStackItem.ConvertObject;
begin
  with Buffer do
  begin
    if TokenObject <> nil then
    begin
    end
    else if TokenStyle = tsDeclare then
    begin
      with TsoDeclare.Create do
      begin
        Name := Token;
        ID := Parser.Data.RegisterID(Name);
{        Stack.Push;
        Block := Items;}
        TokenObject := This;
      end;
      SetFlags([flagDeclare]);
    end
    else if TokenStyle = tsAssign then
    begin
      with TsoAssign.Create do
      begin
        Name := Token;
        ID := Parser.Data.RegisterID(Name);
        TokenObject := This;
      end;
      SetFlags([flagAssign]);
    end
    else
    begin
      if Token = '' then
        RaiseError('Identifier is empty');
      with TsoInstance.Create do
      begin
        Name := Token;
        ID := Parser.Data.RegisterID(Name);
        TokenObject := This;
      end;
      SetFlags([flagObject]);
    end;
  end;
end;

procedure TsrdParser.Flush;
begin
  Stack.Current.Flush;
end;

procedure TsrdParser.TriggerToken(AToken: String; AType: TsrdType);
begin
  Stack.Current.SetIdentifier(AToken, AType);
end;

procedure TsrdParser.TriggerControl(AControl: TsardControl);
begin
  case AControl of
    ctlOpenBlock:
     with TsoSection.Create do
      begin
        Stack.Current.SetObject(This);
        Stack.Push;
        Stack.Current.Block := Items;
      end;
    ctlOpenParams:
      with TsoDescend.Create do
      begin
        Stack.Current.SetObject(This);
        Stack.Push;
        Stack.Current.Block := Items;
      end;

    ctlCloseBlock:
    begin
      Flush;
      Stack.Pop;
      if Stack.Count = 0 then
        RaiseError('Maybe you closed not opened Curly');
    end;
    ctlCloseParams:
    begin
      Flush;
      Stack.Pop;
      if Stack.Count = 0 then
        RaiseError('Maybe you closed not opened Bracket');
    end;
    ctlStart:
    begin
    end;
    ctlStop:
    begin
      Flush;
    end;
    ctlEnd:
    begin
      Flush;
      Stack.Current.NewStatement;
    end;
    ctlAssign:
    begin
      if (Stack.Current.Flags = []) or (Stack.Current.Flags = [flagIdentifier]) then
      begin
        Stack.Current.SetStyle(tsAssign);
        Flush;
      end
      else
        RaiseError('You can not use assignment here!');
    end;
    ctlDeclare:
      begin
        if (Stack.Current.Flags = [flagIdentifier]) then //Only Identifier is opened
        begin
          Stack.Current.SetStyle(tsDeclare);
          Flush;
        end
        else
          RaiseError('You can not use assignment here!');
      end
    else
      RaiseError('Not implemented yet, sorry :(');
  end;
end;

procedure TsrdParser.TriggerOperator(AOperator: TsardObject);
begin
  Flush;
{  if Stack.Current.TokenOperator <> nil then
    RaiseError('Operator already set');}
  Stack.Current.SetOperator(AOperator as TopOperator);
end;

{ TsrdControlScanner }

procedure TsrdControl_Scanner.Scan(const Text: string; var Column: Integer);
var
  aControl: TctlControl;
begin
  aControl := sardEngine.Controls.Scan(Text, Column);
  if aControl <> nil then
    Column := Column + Length(aControl.Name)
  else
    RaiseError('Unkown control started with ' + Text[Column]);

  Scanners.Parser.TriggerControl(aControl.Code);
end;

function TsrdControl_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := sardEngine.IsControl(Text[Column]);
end;

{ TsrdFeeder }

procedure TsrdScanners.Created;
begin
  RegisterScanner(TsrdStart_Scanner);
  RegisterScanner(TsrdWhitespace_Scanner);
  RegisterScanner(TsrdBlockComment_Scanner);
  RegisterScanner(TsrdComment_Scanner);
  RegisterScanner(TsrdLineComment_Scanner);
  RegisterScanner(TsrdNumber_Scanner);
  RegisterScanner(TsrdSQString_Scanner);
  RegisterScanner(TsrdDQString_Scanner);
  RegisterScanner(TsrdControl_Scanner);
  RegisterScanner(TopOperator_Scanner); //Register it after comment because comment take /*
  RegisterScanner(TsrdIdentifier_Scanner);//Last one
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
  aOperator: TopOperator;
begin
  aOperator := sardEngine.Operators.Scan(Text, Column);
  if aOperator <> nil then
    Column := Column + Length(aOperator.Name)
  else
    RaiseError('Unkown operator started with ' + Text[Column]);

  Scanners.Parser.TriggerOperator(aOperator);
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
  Scanners.Parser.TriggerToken(MidStr(Text, c, Column - c), tpObject);
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
  Result := ScanCompare('"', Text, Column);
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
  Result := ScanCompare('''', Text, Column);
end;

{ TsrdBlockCommentScanner }

procedure TsrdBlockComment_Scanner.Scan(const Text: string; var Column: Integer);
begin
  Inc(Column, 2);//2 chars
  while (Column <= Length(Text)) do
  begin
    if (ScanCompare('*/', Text, Column)) then
    begin
      Inc(Column, 2);//2 chars
      break;
    end;
    Inc(Column);
  end;
end;

function TsrdBlockComment_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanCompare('/*', Text, Column);
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
  Result := ScanCompare('//', Text, Column);
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
