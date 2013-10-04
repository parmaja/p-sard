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

  sIdentifierSeparator = '.';

type
  TsrdFlag = (flagNone, flagInstance, flagDeclare, flagAssign, flagIdentifier, flagConst, flagParam, flagOperator, flagComment, flagBlock);
  TsrdFlags = set of TsrdFlag;

  { TsrdFeeder }

  TsrdFeeder = class(TsardFeeder)
  protected
    procedure DoStart; override;
    procedure DoStop; override;
  end;

  { TsrdLexer }

  TsrdLexer = class(TsardLexer)
  private
    FControls: TctlControls;
    FOperators: TopOperators;
  protected
    procedure Created; override;
  public
    constructor Create(vParser: TsardParser);
    destructor Destroy; override;
    function IsWhiteSpace(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;
    function IsControl(vChar: AnsiChar): Boolean; override;
    function IsOperator(vChar: AnsiChar): Boolean; override;
    function IsNumber(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;
    function IsIdentifier(vChar: AnsiChar; vOpen: Boolean = True): Boolean; override;

    property Operators: TopOperators read FOperators;
    property Controls: TctlControls read FControls;
  end;

  TsrdParser = class;

  TsrdObjectStyle = (tsConst, tsInstance, tsDeclare, tsAssign, tsBlock);

  { TsrdInterpret }

  TsrdInterpret = class abstract(TsardObject)
  private
    FFlags: TsrdFlags;
    FFlag: TsrdFlag;
  protected
    Identifier: string;
    TokenOperator: TopOperator;
    TokenObject: TsoObject;

    Parser: TsrdParser;
    Block: TsrdBlock;//TODO move it to TsrdInterpretBlock
    Statement: TsrdStatement;
  public
    constructor Create(AParser: TsrdParser);
    destructor Destroy; override;

    procedure SetOperator(AOperator: TopOperator);
    procedure SetIdentifier(AIdentifier: string);
    procedure SetNumber(AIdentifier: string);
    procedure SetString(AIdentifier: string);
    procedure SetComment(AIdentifier: string);
    procedure SetInstance(AIdentifier: string);
    procedure SetInstance;
    procedure SetDeclare;
    procedure SetAssign;
    procedure SetObject(AObject: TsoObject);
    procedure SetFlag(AFlag: TsrdFlag);
    procedure CheckStatement;
    procedure Flush; virtual;
    procedure EndStatement; virtual; abstract;
    property Flags: TsrdFlags read FFlags;
    property Flag: TsrdFlag read FFlag;
  end;

  TsrdInterpretClass = class of TsrdInterpret;

  TsrdInterpretBlock = class (TsrdInterpret)
  public
    procedure EndStatement; override;
  end;

  { TsrdInterpretStatement }

  TsrdInterpretStatement = class (TsrdInterpret)
  public
    procedure EndStatement; override;
  end;

  TsrdInterpretParams = class (TsrdInterpret)
  public
  end;

  { TsrdInterpreter }

  TsrdInterpreter = class(TsardStack)
  private
    function GetCurrent: TsrdInterpret;
  protected
    Parser: TsrdParser;
  public
    procedure Push(vItem: TsrdInterpret);
    function Push(vItemClass: TsrdInterpretClass): TsrdInterpret;
    constructor Create(AParser: TsrdParser);
    property Current: TsrdInterpret read GetCurrent;
  end;

  { TsrdFeederParser }

  TsrdParser = class(TsardParser)
  protected
    FStack: TsrdInterpreter;
    FData: TrunData;
  public
    procedure Start; override;
    procedure Stop; override;
    constructor Create(AData: TrunData; ABlock: TsrdBlock);
    destructor Destroy; override;

    procedure TriggerToken(AToken: String; AType: TsrdType); override;
    procedure TriggerOperator(AOperator: TsardObject); override;
    procedure TriggerControl(AControl: TsardControl); override;
    procedure Flush;
    property Stack: TsrdInterpreter read FStack;
    property Data: TrunData read FData;
  end;

{------  Scanners Objects ------}

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

{ TsrdInterpretBlock }

procedure TsrdInterpretBlock.EndStatement;
begin
  Statement := nil;
  FFlags := [];
  FFlag := flagNone;
end;

{ TsrdInterpretStatement }

procedure TsrdInterpretStatement.EndStatement;
begin
  Statement := nil;
  FFlags := [];
  FFlag := flagNone;
end;

{ TsrdComment_Scanner }

procedure TsrdComment_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  Inc(Column, 2);//2 chars
  c := Column;
  while (Column <= Length(Text)) do
  begin
    if (ScanCompare('*}', Text, Column)) then
    begin
      Lexer.Parser.TriggerToken(MidStr(Text, c, Column - c), tpComment);
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

{ TsrdFeeder }

procedure TsrdFeeder.DoStart;
begin
  inherited;
  Lexer.Parser.TriggerControl(ctlStart);
end;

procedure TsrdFeeder.DoStop;
begin
  inherited;
  Lexer.Parser.TriggerControl(ctlStop);
end;

{ TsrdInterpret }

procedure TsrdInterpret.SetIdentifier(AIdentifier: string);
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  Identifier := AIdentifier;
  SetFlag(flagIdentifier);
end;

procedure TsrdInterpret.SetNumber(AIdentifier: string);
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  if pos('.', AIdentifier) > 0 then
  begin
    with TsoFloat.Create do
    begin
      Value := StrToFloat(AIdentifier);
      TokenObject := This;
    end;
  end
  else
  begin
    with TsoInteger.Create do
    begin
      Value := StrToInt64(AIdentifier);
      TokenObject := This;
    end;
  end;
  SetFlag(flagConst);
end;

procedure TsrdInterpret.SetString(AIdentifier: string);
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  with TsoString.Create do
  begin
    Value := AIdentifier;
    TokenObject := This;
  end;
  SetFlag(flagConst);
end;

procedure TsrdInterpret.SetComment(AIdentifier: string);
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  with TsoComment.Create do
  begin
    Value := AIdentifier;
    TokenObject := This;
  end;
  SetFlag(flagComment);
end;

procedure TsrdInterpret.SetDeclare;
begin
  CheckStatement;
  with TsoDeclare.Create do
  begin
    Name := Identifier;
    ID := Parser.Data.RegisterID(Name);
    TokenObject := This;
    Statement := Self.Statement;
  end;
  Identifier := '';
  SetFlag(flagDeclare);
end;

procedure TsrdInterpret.SetAssign;
begin
  with TsoAssign.Create do
  begin
    Name := Identifier;
    ID := Parser.Data.RegisterID(Name);
    TokenObject := This;
  end;
  Identifier := '';
  SetFlag(flagAssign);
end;

procedure TsrdInterpret.SetInstance(AIdentifier: string);
begin
  with TsoInstance.Create do
  begin
    Name := AIdentifier;
    ID := Parser.Data.RegisterID(Name);
    TokenObject := This;
  end;
  SetFlag(flagInstance);
end;

procedure TsrdInterpret.SetInstance;
begin
  SetInstance(Identifier);
  Identifier := '';
end;

procedure TsrdInterpret.SetOperator(AOperator: TopOperator);
begin
  Flush;
  if TokenOperator <> nil then
    RaiseError('Operator is already set');
  TokenOperator := AOperator;
end;

procedure TsrdInterpret.SetObject(AObject: TsoObject);
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  if TokenObject <> nil then
    RaiseError('Object is already set');
  TokenObject := AObject;
end;

procedure TsrdInterpret.SetFlag(AFlag: TsrdFlag);
begin
  FFlags := FFlags + [AFlag];
  FFlag := AFlag;
end;

procedure TsrdInterpret.CheckStatement;
begin
  if Statement = nil then
  begin
    if Block = nil then
      RaiseError('Maybe you need to set a block, or it single statment block');
    Statement := Block.Add;
  end;
end;

procedure TsrdInterpret.Flush;
begin
  if Identifier <> '' then
  begin
    SetInstance(Identifier);
    Identifier := '';
  end;
  if (TokenObject <> nil) or (TokenOperator <> nil) then
  begin
    if Identifier <> '' then
      RaiseError('Identifier is already set, can not flush');
    if TokenObject = nil then
      RaiseError('Object is nil!');
    CheckStatement;
    Statement.Add(TokenOperator, TokenObject);
    TokenObject := nil;
    TokenOperator := nil;
  end;
end;

constructor TsrdInterpret.Create(AParser: TsrdParser);
begin
  inherited Create;
  Parser := AParser;
end;

destructor TsrdInterpret.Destroy;
begin
  inherited;
end;

{ TsrdInterpreter }

function TsrdInterpreter.GetCurrent: TsrdInterpret;
begin
  Result := (inherited GetCurrent) as TsrdInterpret;
end;

procedure TsrdInterpreter.Push(vItem: TsrdInterpret);
begin
  inherited Push(vItem);
end;

function TsrdInterpreter.Push(vItemClass: TsrdInterpretClass): TsrdInterpret;
begin
  Result := vItemClass.Create(Parser);
  Push(Result);
end;

constructor TsrdInterpreter.Create(AParser: TsrdParser);
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
  FStack := TsrdInterpreter.Create(Self);
  if ABlock <> nil then
    with Stack.Push(TsrdInterpretBlock) do
    begin
       Block := ABlock;
    end;
end;

destructor TsrdParser.Destroy;
begin
  FreeAndNil(FStack);
  inherited Destroy;
end;

procedure TsrdParser.Flush;
begin
  Stack.Current.Flush;
end;

procedure TsrdParser.TriggerToken(AToken: String; AType: TsrdType);
begin
  case AType of
    tpNumber: Stack.Current.SetNumber(AToken);
    tpString: Stack.Current.SetString(AToken);
    tpComment: Stack.Current.SetComment(AToken);
    else
       Stack.Current.SetIdentifier(AToken);
  end
end;

procedure TsrdParser.TriggerControl(AControl: TsardControl);
begin
  case AControl of
    ctlOpenBlock:
     with TsoSection.Create do
      begin
        Stack.Current.SetObject(This);
        Stack.Push(TsrdInterpretBlock);
        Stack.Current.Block := Items;
      end;
    ctlCloseBlock:
      begin
        Flush;
        Stack.Pop;
        if Stack.Count = 0 then
          RaiseError('Maybe you closed not opened Curly');
      end;
    ctlOpenParams:
      begin
        //here we add block to TsoInstance if there is indienifier opened witout operator
        if Stack.Current.Flags = [flagDeclare, flagIdentifier] then
        begin
          Stack.Push(TsrdInterpretParams);
        end
        else
        if Stack.Current.Flags = [flagIdentifier] then
        begin
          Stack.Current.SetInstance;
          Stack.Push(TsrdInterpretBlock);
          Stack.Current.Block := (Stack.Current.TokenObject as TsoBlock).Items;
        end
        else
        with TsoStatement.Create do
        begin
          Stack.Current.SetObject(This);
          Stack.Push(TsrdInterpretStatement);
          Stack.Current.Statement := Statement;
        end;
      end;
    ctlCloseParams:
      begin
        Flush;
        Stack.Pop;
        if Stack.Count = 0 then
          RaiseError('Maybe you closed not opened Bracket');
      end;
    ctlAssign:
      begin
        if (Stack.Current.Flags = []) or (Stack.Current.Flags = [flagIdentifier]) then
        begin
          Stack.Current.SetAssign;
          Flush;
        end
        else
          RaiseError('You can not use assignment here!');
      end;
    ctlDeclare:
      begin
        if (Stack.Current.Flags = [flagIdentifier]) then
        begin
          Stack.Current.SetDeclare;
          Flush;
        end
        else
          RaiseError('You can not use assignment here!');
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
        Stack.Current.EndStatement;
      end;
    ctlNext:
      begin
        Flush;
        Stack.Current.EndStatement;
      end;
    else
      RaiseError('Not implemented yet, sorry :(');
  end;
end;

procedure TsrdParser.TriggerOperator(AOperator: TsardObject);
begin
  Stack.Current.SetOperator(AOperator as TopOperator);
end;

{ TsrdControlScanner }

procedure TsrdControl_Scanner.Scan(const Text: string; var Column: Integer);
var
  aControl: TctlControl;
begin
  aControl := (Lexer as TsrdLexer).Controls.Scan(Text, Column);
  if aControl <> nil then
    Column := Column + Length(aControl.Name)
  else
    RaiseError('Unkown control started with ' + Text[Column]);

  Lexer.Parser.TriggerControl(aControl.Code);
end;

function TsrdControl_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexer.IsControl(Text[Column]);
end;

{ TsrdFeeder }

procedure TsrdLexer.Created;
begin
  with Controls do
  begin
    Add('(', ctlOpenParams);
    Add('[', ctlOpenArray);
    Add('{', ctlOpenBlock);
    Add(')', ctlCloseParams);
    Add(']', ctlCloseArray);
    Add('}', ctlCloseBlock);
    Add(';', ctlEnd);
    Add(',', ctlEnd);
    Add(':', ctlDeclare);
    Add(':=', ctlAssign);
  end;

  with Operators do
  begin
    Add(TopPlus);
    Add(TopMinus);
    Add(TopMultiply);
    Add(TopDivide);

    Add(TopEqual);
    Add(TopNotEqual);
    Add(TopAnd);
    Add(TopOr);
    Add(TopNot);

    Add(TopGreater);
    Add(TopLesser);

    Add(TopPower);
  end;

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

constructor TsrdLexer.Create(vParser: TsardParser);
begin
  inherited Create(vParser);
  FOperators := TopOperators.Create;
  FControls := TctlControls.Create;
end;

destructor TsrdLexer.Destroy;
begin
  FreeAndNil(FControls);
  FreeAndNil(FOperators);
  inherited Destroy;
end;

function TsrdLexer.IsWhiteSpace(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  Result := vChar in sWhitespace;
end;

function TsrdLexer.IsControl(vChar: AnsiChar): Boolean;
begin
  Result := Controls.IsOpenBy(vChar);
end;

function TsrdLexer.IsOperator(vChar: AnsiChar): Boolean;
begin
  Result := Operators.IsOpenBy(vChar);
end;

function TsrdLexer.IsNumber(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  if vOpen then
    Result := vChar in sNumberOpenChars
  else
    Result := vChar in sNumberChars;
end;

function TsrdLexer.IsIdentifier(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  Result := inherited IsIdentifier(vChar, vOpen);
end;

{ TsrdNumberScanner }

procedure TsrdNumber_Scanner.Scan(const Text: string; var Column: Integer);
var
  l, c: Integer;
begin
  c := Column;
  l := Length(Text);
  while (Column <= l) and (Lexer.IsNumber(Text[Column], False)) do
    Inc(Column);
  Lexer.Parser.TriggerToken(MidStr(Text, c, Column - c), tpNumber);
end;

function TsrdNumber_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexer.IsNumber(Text[Column], True);//need to improve to accept unicode chars
end;

{ TopOperatorScanner }

procedure TopOperator_Scanner.Scan(const Text: string; var Column: Integer);
var
  aOperator: TopOperator;
begin
  aOperator := (Lexer as TsrdLexer).Operators.Scan(Text, Column);
  if aOperator <> nil then
    Column := Column + Length(aOperator.Name)
  else
    RaiseError('Unkown operator started with ' + Text[Column]);

  Lexer.Parser.TriggerOperator(aOperator);
end;

function TopOperator_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexer.IsOperator(Text[Column]);
end;

{ TsrdIdentifierScanner }

procedure TsrdIdentifier_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (Lexer.IsIdentifier(Text[Column], False)) do
    Inc(Column);
  Lexer.Parser.TriggerToken(MidStr(Text, c, Column - c), tpIdentifier);
end;

function TsrdIdentifier_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexer.IsIdentifier(Text[Column], True);
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
  Lexer.Parser.TriggerToken(MidStr(Text, c, Column - c), tpString);
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
  Lexer.Parser.TriggerToken(MidStr(Text, c, Column - c), tpString);
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
