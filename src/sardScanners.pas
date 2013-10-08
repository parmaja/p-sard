unit sardScanners;
{**
 *  This file is part of the "SARD"
 *
 * @license   The MIT License (MIT)
 *            Included in this distribution
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}{$M+}

(*
  #Like pascal
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

  #Like C
    Block { }, no more begin end
    comments //single line and /* multiline */
    Not "!"  or "|"

  -----------------------------------------------------

  #Like nothing
    Returning value

   foo:{
     := 10;
   }

   or

   foo:integer(p1:integer){
     := 10;
   }

   Description: new comment but saved with the objects.
   {* This object not usfule dont use it *};

   With:
     object.{     <-not sure
   }

  Escape char is outside the string there is no escape inside it. sorry for that.
  "test " \" " and"
  it equal to "test " and"

*)

{TODO:
  how to scan, minus here?
  x := -10;
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

  TsrdStatementType = (stmNormal, stmAssign, stmDeclare);

  TsrdParser = class;
  TsrdInterpreter = class;

  { TsrdInterpreter }

  TsrdInterpreter = class(TsardObject)
  private
    FFlags: TsrdFlags;
    FFlag: TsrdFlag;
  protected
    Identifier: string;
    TokenOperator: TopOperator;
    TokenObject: TsoObject;

    Parser: TsrdParser;
    Statement: TsrdStatement;
    StatementType: TsrdStatementType;
  public
    constructor Create(AParser: TsrdParser);
    destructor Destroy; override;

    procedure SetOperator(AOperator: TopOperator);
    procedure SetIdentifier(AIdentifier: string);
    function SetNumber(AIdentifier: string): TsoNumber;
    function SetString(AIdentifier: string): TsoString;
    function SetComment(AIdentifier: string): TsoComment;
    function SetInstance(AIdentifier: string): TsoInstance;
    function SetInstance: TsoInstance;
    function SetDeclare: TsoDeclare;
    function SetAssign: TsoAssign;
    procedure SetObject(AObject: TsoObject);
    procedure SetFlag(AFlag: TsrdFlag);

    procedure Push(vItem: TsrdInterpreter);
    procedure Pop;
    procedure Flush; virtual;
    procedure PrepareStatement; virtual;
    procedure EndStatement; virtual;

    procedure TriggerToken(AToken: String; AType: TsrdType); virtual;
    procedure TriggerOperator(AOperator: TopOperator); virtual;

    property Flags: TsrdFlags read FFlags;
    property Flag: TsrdFlag read FFlag;
  end;

  TsrdParseClass = class of TsrdInterpreter;

  { TsrdParseInit }

  TsrdInterpreterInit = class (TsrdInterpreter)
  protected
    Block: TsrdBlock;
  public
    constructor Create(AParser: TsrdParser; ABlock: TsrdBlock);
    procedure PrepareStatement; override;
  end;

  { TsrdParseBlock }

  TsrdInterpreterBlock = class (TsrdInterpreter)
  protected
    Block: TsrdBlock;
  public
    constructor Create(AParser: TsrdParser; ABlock: TsrdBlock);
    procedure PrepareStatement; override;
  end;

  { TsrdParseStatement }

  TsrdInterpreterStatement = class (TsrdInterpreter)
  public
    constructor Create(AParser: TsrdParser; AStatement: TsrdStatement);
    procedure EndStatement; override;
  end;

  { TsrdControler }

  TsrdControler = class(TsardObject)
  protected
    Parser: TsrdParser;
  public
    constructor Create(AParser: TsrdParser);
    procedure Control(AControl: TsardControl); virtual; abstract;
  end;

  { TsrdControlers }

  TsrdControlers = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TsrdControler;
  protected
  public
    procedure Add(AControler: TsrdControler);
    property Items[Index: Integer]: TsrdControler read GetItem; default;
  end;

  { TsrdControlerNormal }

  TsrdControlerNormal = class(TsrdControler)
  public
    procedure Control(AControl: TsardControl); override;
  end;

  { TsrdControlerAssign }

  TsrdControlerAssign = class(TsrdControlerNormal)
  public
    procedure Control(AControl: TsardControl); override;
  end;

  { TstdControlDeclare }

  TsrdControlerDeclare = class(TsrdControlerNormal)
  public
    procedure Control(AControl: TsardControl); override;
  end;

  { TsrdParser }

  TsrdParser = class(TsardParser)
  protected
    FData: TrunData;
    function GetCurrent: TsrdInterpreter;
    procedure Created; override;
  public
    Controler: TsrdControler;//=nil
    Controlers: TsrdControlers;
    PopItem: Boolean;
    constructor Create(AData: TrunData; ABlock: TsrdBlock);
    destructor Destroy; override;

    property Current: TsrdInterpreter read GetCurrent;
    procedure Push(vItem: TsrdInterpreter);
    function Push(vItemClass: TsrdParseClass): TsrdInterpreter;

    procedure Start; override;
    procedure Stop; override;

    procedure TriggerToken(AToken: String; AType: TsrdType); override;
    procedure TriggerOperator(AOperator: TsardObject); override;
    procedure TriggerControl(AControl: TsardControl); override;
    property Data: TrunData read FData;
  end;

  {------  Scanners Objects ------}


  { TsrdFeeder }

  TsrdFeeder = class(TsardFeeder)
  protected
    procedure DoStart; override;
    procedure DoStop; override;
  end;

  { TsrdLexical }

  TsrdLexical = class(TsardLexical)
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

  { TsrdWhitespaceScanner }

  TsrdWhitespace_Scanner = class(TsardScanner)
  protected
    function Scan(const Text: string; var Column: Integer): Boolean;  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsrdIdentifierScanner }

  TsrdIdentifier_Scanner = class(TsardScanner)
  protected
    function Scan(const Text: string; var Column: Integer): Boolean;  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsrdNumberScanner }

  TsrdNumber_Scanner = class(TsardScanner)
  protected
    function Scan(const Text: string; var Column: Integer): Boolean;  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsrdControl_Scanner }

  TsrdControl_Scanner = class(TsardScanner)
  protected
    function Scan(const Text: string; var Column: Integer): Boolean;  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TopOperatorScanner }

  TopOperator_Scanner = class(TsardScanner)
  protected
    function Scan(const Text: string; var Column: Integer): Boolean;  override;
    function Accept(const Text: string; var Column: Integer): Boolean;  override;
  end;

  { TsrdLineCommentScanner }

  TsrdLineComment_Scanner = class(TsardScanner)
  protected
    function Scan(const Text: string; var Column: Integer): Boolean;  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsrdBlockCommentScanner }

  TsrdBlockComment_Scanner = class(TsardScanner)
  protected
    function Scan(const Text: string; var Column: Integer): Boolean;  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsrdComment_Scanner }
  { it is stored with compiled objects}

  TsrdComment_Scanner = class(TsardScanner)
  protected
    Buffer: string;
    function Scan(const Text: string; var Column: Integer): Boolean;  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsrdSQStringScanner }

  TsrdString_Scanner = class abstract(TsardScanner)
  protected
    QuoteChar: Char;
    Buffer: string;//<- not sure it is good idea
    function Scan(const Text: string; var Column: Integer): Boolean;  override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TsrdSQString_Scanner }

  TsrdSQString_Scanner = class(TsrdString_Scanner)
  protected
    procedure Created; override;
  end;

  { TsrdDQString_Scanner }

  TsrdDQString_Scanner = class(TsrdString_Scanner)
  protected
    procedure Created; override;
  end;

implementation

uses
  StrUtils;

{ TstdControlDeclare }

procedure TsrdControlerDeclare.Control(AControl: TsardControl);
begin
  inherited Control(AControl);
end;

{ TstdControlAssign }

procedure TsrdControlerAssign.Control(AControl: TsardControl);
begin
  inherited;
{  with Parser.Current do
    case AControl of
    end;}
end;

{ TsrdControlerNormal }

procedure TsrdControlerNormal.Control(AControl: TsardControl);
begin
  with Parser.Current do
  case AControl of
    ctlAssign:
      begin
        if (Flags = []) or (Flags = [flagIdentifier]) then
        begin
          SetAssign;
          Flush;
        end
        else
          RaiseError('You can not use assignment here!');
      end;
    ctlDeclare:
      begin
        if (Flags = [flagIdentifier]) then
        begin
          SetDeclare;
        end
        else
          RaiseError('You can not use assignment here!');
      end;

    ctlOpenBlock:
      begin
        with TsoSection.Create do
         begin
           SetObject(This);
           Push(TsrdInterpreterInit.Create(Parser, Items));
         end;
      end;
    ctlCloseBlock:
      begin
        Flush;
        if Parser.Count = 1 then
          RaiseError('Maybe you closed not opened Curly');
        Pop;
      end;
    ctlOpenParams:
      begin
        //here we add block to TsoInstance if there is indienifier opened witout operator
        if Flags = [flagDeclare, flagIdentifier] then
        begin
          //Switch Controler
        end
        else
        if Flags = [flagIdentifier] then
        begin
          Push(TsrdInterpreterBlock.Create(Parser, SetInstance.Items));
        end
        else
        with TsoStatement.Create do
        begin
          SetObject(This);
          Push(TsrdInterpreterStatement.Create(Parser, Statement));
        end;
      end;
    ctlCloseParams:
      begin
        Flush;
        if Parser.Count = 1 then
          RaiseError('Maybe you closed not opened Bracket');
        Pop;
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
        EndStatement;
      end;
    ctlNext:
      begin
        Flush;
        EndStatement;
      end;
    else
      RaiseError('Not implemented yet, sorry :(');
  end;
end;

{ TsrdControlers }

function TsrdControlers.GetItem(Index: Integer): TsrdControler;
begin
  Result := inherited Items[Index] as TsrdControler;
end;

procedure TsrdControlers.Add(AControler: TsrdControler);
begin
  inherited Add(AControler);
end;

{ TsrdControler }

constructor TsrdControler.Create(AParser: TsrdParser);
begin
  inherited Create;
  Parser := AParser;
end;

{ TsrdSQString_Scanner }

procedure TsrdSQString_Scanner.Created;
begin
  inherited Created;
  QuoteChar := '''';
end;

{ TsrdInterpreterInit }

constructor TsrdInterpreterInit.Create(AParser: TsrdParser; ABlock: TsrdBlock);
begin
  inherited Create(AParser);
  Block := ABlock;
end;

procedure TsrdInterpreterInit.PrepareStatement;
begin
  if Statement = nil then
  begin
    if Block = nil then
      RaiseError('Maybe you need to set a block, or it single statment block');
    Statement := Block.Add;
  end;
end;

{ TsrdInterpreterBlock }

procedure TsrdInterpreterBlock.PrepareStatement;
begin
  if Statement = nil then
  begin
    if Block = nil then
      RaiseError('Maybe you need to set a block, or it single statment block');
    Statement := Block.Add;
  end;
end;

constructor TsrdInterpreterBlock.Create(AParser: TsrdParser; ABlock: TsrdBlock);
begin
  inherited Create(AParser);
  Block := ABlock;
end;

{ TsrdInterpreterStatement }

constructor TsrdInterpreterStatement.Create(AParser: TsrdParser; AStatement: TsrdStatement);
begin
  inherited Create(AParser);
  Statement := AStatement;
end;

procedure TsrdInterpreterStatement.EndStatement;
begin
  Statement := nil;
  FFlags := [];
  FFlag := flagNone;
end;

{ TsrdComment_Scanner }

function TsrdComment_Scanner.Scan(const Text: string; var Column: Integer): Boolean;
var
  c: Integer;
begin
  c := Column;
  Result := False;
  while (Column <= Length(Text)) do
  begin
    if (ScanCompare('*}', Text, Column)) then
    begin
      Buffer := Buffer + MidStr(Text, c, Column - c);
      Lexical.Parser.TriggerToken(Buffer, tpComment);
      Result := True;
      Buffer := '';
      Inc(Column, 2);//2 chars
      break;
    end;
    Inc(Column);
  end;
  if not Result then
    Buffer := Buffer + MidStr(Text, c, Column - c);
end;

function TsrdComment_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanCompare('{*', Text, Column);
  if Result then
    Inc(Column, 2);//2 chars
end;

{ TsrdFeeder }

procedure TsrdFeeder.DoStart;
begin
  inherited;
  Lexical.Parser.TriggerControl(ctlStart);
end;

procedure TsrdFeeder.DoStop;
begin
  inherited;
  Lexical.Parser.TriggerControl(ctlStop);
end;

{ TsrdInterpreter }

procedure TsrdInterpreter.SetIdentifier(AIdentifier: string);
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  Identifier := AIdentifier;
  SetFlag(flagIdentifier);
end;

function TsrdInterpreter.SetNumber(AIdentifier: string): TsoNumber;
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  if (pos('.', AIdentifier) > 0) or ((pos('E', AIdentifier) > 0)) then
    Result := TsoFloat.Create(StrToFloat(AIdentifier))
  else
    Result := TsoInteger.Create(StrToInt64(AIdentifier));
  TokenObject := Result;
  SetFlag(flagConst);
end;

function TsrdInterpreter.SetString(AIdentifier: string): TsoString;
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  Result := TsoString.Create;
  with Result do
  begin
    Value := AIdentifier;
  end;
  TokenObject := Result;
  SetFlag(flagConst);
end;

function TsrdInterpreter.SetComment(AIdentifier: string): TsoComment;
begin
  //We need to check if it the first expr in the statment
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  Result := TsoComment.Create;
  with Result do
  begin
    Value := AIdentifier;
  end;
  TokenObject := Result;
  SetFlag(flagComment);
end;

function TsrdInterpreter.SetDeclare: TsoDeclare;
begin
  if Statement <> nil then
    RaiseError('Declare must be the first in a statement');
  Result := TsoDeclare.Create;
  with Result do
  begin
    Name := Identifier;
    ID := Parser.Data.RegisterID(Name);
  end;
  TokenObject := Result;
  Identifier := '';
  SetFlag(flagDeclare);
  Flush;
  Statement := Result.Statement;
end;

function TsrdInterpreter.SetAssign: TsoAssign;
begin
  Result := TsoAssign.Create;
  with Result do
  begin
    Name := Identifier;
    ID := Parser.Data.RegisterID(Name);
  end;
  TokenObject := Result;
  Identifier := '';
  SetFlag(flagAssign);
end;

function TsrdInterpreter.SetInstance(AIdentifier: string):TsoInstance;
begin
  Result := TsoInstance.Create;
  with Result do
  begin
    Name := AIdentifier;
    ID := Parser.Data.RegisterID(Name);
  end;
  TokenObject := Result;
  SetFlag(flagInstance);
end;

function TsrdInterpreter.SetInstance: TsoInstance;
begin
  Result := SetInstance(Identifier);
  Identifier := '';
end;

procedure TsrdInterpreter.SetOperator(AOperator: TopOperator);
begin
  Flush;
  if TokenOperator <> nil then
    RaiseError('Operator is already set');
  TokenOperator := AOperator;
end;

procedure TsrdInterpreter.SetObject(AObject: TsoObject);
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  if TokenObject <> nil then
    RaiseError('Object is already set');
  TokenObject := AObject;
end;

procedure TsrdInterpreter.SetFlag(AFlag: TsrdFlag);
begin
  FFlags := FFlags + [AFlag];
  FFlag := AFlag;
end;

procedure TsrdInterpreter.PrepareStatement;
begin
end;

procedure TsrdInterpreter.Push(vItem: TsrdInterpreter);
begin
  Parser.Push(vItem);
end;

procedure TsrdInterpreter.Pop;
begin
  Parser.PopItem := True;
end;

procedure TsrdInterpreter.Flush;
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
    PrepareStatement;
    Statement.Add(TokenOperator, TokenObject);
    TokenObject := nil;
    TokenOperator := nil;
  end;
end;

procedure TsrdInterpreter.EndStatement;
begin
  Statement := nil;
  FFlags := [];
  FFlag := flagNone;
end;

procedure TsrdInterpreter.TriggerToken(AToken: String; AType: TsrdType);
begin
  case AType of
    tpNumber: SetNumber(AToken);
    tpString: SetString(AToken);
    tpComment: SetComment(AToken);
    else
       SetIdentifier(AToken);
  end
end;

procedure TsrdInterpreter.TriggerOperator(AOperator: TopOperator);
begin
  SetOperator(AOperator);
end;

constructor TsrdInterpreter.Create(AParser: TsrdParser);
begin
  inherited Create;
  Parser := AParser;
end;

destructor TsrdInterpreter.Destroy;
begin
  inherited;
end;

function TsrdParser.GetCurrent: TsrdInterpreter;
begin
  Result := (inherited GetCurrent) as TsrdInterpreter;
end;

procedure TsrdParser.Created;
begin
  inherited Created;
  Controlers.Add(TsrdControlerNormal.Create(Self));
  Controlers.Add(TsrdControlerAssign.Create(Self));
  Controlers.Add(TsrdControlerDeclare.Create(Self));
  Controler := Controlers[0];
end;

procedure TsrdParser.Push(vItem: TsrdInterpreter);
begin
  inherited Push(vItem);
end;

function TsrdParser.Push(vItemClass: TsrdParseClass): TsrdInterpreter;
begin
  Result := vItemClass.Create(Self);
  Push(Result);
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
  Controlers := TsrdControlers.Create;
  FData := AData;
  if ABlock <> nil then
    Push(TsrdInterpreterInit.Create(Self, ABlock));
end;

destructor TsrdParser.Destroy;
begin
  FreeAndNil(Controlers);
  inherited;
end;

procedure TsrdParser.TriggerToken(AToken: String; AType: TsrdType);
begin
  Current.TriggerToken(AToken, AType);
  if PopItem then
  begin
    PopItem := False;
    Pop;
  end;
end;

procedure TsrdParser.TriggerControl(AControl: TsardControl);
begin
  Controler.Control(AControl);
  //Current.TriggerControl(AControl);
  if PopItem then
  begin
    PopItem := False;
    Pop;
  end;
end;

procedure TsrdParser.TriggerOperator(AOperator: TsardObject);
begin
  Current.TriggerOperator(AOperator as TopOperator);
  if PopItem then
  begin
    PopItem := False;
    Pop;
  end;
end;

{ TsrdControlScanner }

function TsrdControl_Scanner.Scan(const Text: string; var Column: Integer): Boolean;
var
  aControl: TctlControl;
begin
  aControl := (Lexical as TsrdLexical).Controls.Scan(Text, Column);
  if aControl <> nil then
    Column := Column + Length(aControl.Name)
  else
    RaiseError('Unkown control started with ' + Text[Column]);

  Lexical.Parser.TriggerControl(aControl.Code);
  Result := True;
end;

function TsrdControl_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexical.IsControl(Text[Column]);
end;

{ TsrdFeeder }

procedure TsrdLexical.Created;
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

  AddScanner(TsrdWhitespace_Scanner);
  AddScanner(TsrdBlockComment_Scanner);
  AddScanner(TsrdComment_Scanner);
  AddScanner(TsrdLineComment_Scanner);
  AddScanner(TsrdNumber_Scanner);
  AddScanner(TsrdSQString_Scanner);
  AddScanner(TsrdDQString_Scanner);
  AddScanner(TsrdControl_Scanner);
  AddScanner(TopOperator_Scanner); //Register it after comment because comment take /*
  AddScanner(TsrdIdentifier_Scanner);//Last one
end;

constructor TsrdLexical.Create(vParser: TsardParser);
begin
  inherited Create(vParser);
  FOperators := TopOperators.Create;
  FControls := TctlControls.Create;
end;

destructor TsrdLexical.Destroy;
begin
  FreeAndNil(FControls);
  FreeAndNil(FOperators);
  inherited Destroy;
end;

function TsrdLexical.IsWhiteSpace(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  Result := vChar in sWhitespace;
end;

function TsrdLexical.IsControl(vChar: AnsiChar): Boolean;
begin
  Result := Controls.IsOpenBy(vChar);
end;

function TsrdLexical.IsOperator(vChar: AnsiChar): Boolean;
begin
  Result := Operators.IsOpenBy(vChar);
end;

function TsrdLexical.IsNumber(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  if vOpen then
    Result := vChar in sNumberOpenChars
  else
    Result := vChar in sNumberChars;
end;

function TsrdLexical.IsIdentifier(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  Result := inherited IsIdentifier(vChar, vOpen);
end;

{ TsrdNumberScanner }

function TsrdNumber_Scanner.Scan(const Text: string; var Column: Integer): Boolean;
var
  l, c: Integer;
begin
  c := Column;
  l := Length(Text);
  while (Column <= l) and (Lexical.IsNumber(Text[Column], False)) do
    Inc(Column);
  Lexical.Parser.TriggerToken(MidStr(Text, c, Column - c), tpNumber);
  Result := True;
end;

function TsrdNumber_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexical.IsNumber(Text[Column], True);//need to improve to accept unicode chars
end;

{ TopOperatorScanner }

function TopOperator_Scanner.Scan(const Text: string; var Column: Integer): Boolean;
var
  aOperator: TopOperator;
begin
  aOperator := (Lexical as TsrdLexical).Operators.Scan(Text, Column);
  if aOperator <> nil then
    Column := Column + Length(aOperator.Name)
  else
    RaiseError('Unkown operator started with ' + Text[Column]);

  Lexical.Parser.TriggerOperator(aOperator);
  Result := True;
end;

function TopOperator_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexical.IsOperator(Text[Column]);
end;

{ TsrdIdentifierScanner }

function TsrdIdentifier_Scanner.Scan(const Text: string; var Column: Integer): Boolean;
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (Lexical.IsIdentifier(Text[Column], False)) do
    Inc(Column);
  Lexical.Parser.TriggerToken(MidStr(Text, c, Column - c), tpIdentifier);
  Result := True;
end;

function TsrdIdentifier_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexical.IsIdentifier(Text[Column], True);
end;

{ TsrdDQStringScanner }

procedure TsrdDQString_Scanner.Created;
begin
  inherited Created;
  QuoteChar := '"';
end;

{ TsrdSQStringScanner }

function TsrdString_Scanner.Scan(const Text: string; var Column: Integer): Boolean;
var
  c: Integer;
begin
  c := Column;
  Result := False;
  while (Column <= Length(Text)) do
  begin
    if (Text[Column] = QuoteChar) then  //TODO Escape, not now
    begin
      Buffer := Buffer + MidStr(Text, c, Column - c);
      Lexical.Parser.TriggerToken(Buffer, tpString);
      Result := True;
      Buffer := '';
      Inc(Column);
      break;
    end;
    Inc(Column);
  end;
  if not Result then
    Buffer := Buffer + MidStr(Text, c, Column - c);
end;

function TsrdString_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanCompare(QuoteChar, Text, Column);
  if Result then
    Inc(Column, Length(QuoteChar));
end;

{ TsrdBlockCommentScanner }

function TsrdBlockComment_Scanner.Scan(const Text: string; var Column: Integer): Boolean;
begin
  Result := False;
  while (Column <= Length(Text)) do
  begin
    if (ScanCompare('*/', Text, Column)) then
    begin
      Inc(Column, 2);//2 chars
      Result := True;
      break;
    end;
    Inc(Column);
  end;
end;

function TsrdBlockComment_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanCompare('/*', Text, Column);
  if Result then
    Inc(Column, 2);//2 chars
end;

{ TsrdLineComment_Scanner }

function TsrdLineComment_Scanner.Scan(const Text: string; var Column: Integer): Boolean;
begin
  while (Column <= Length(Text)) and not (Text[Column] in sEOL) do //TODO ignore quoted strings
    Inc(Column);
  Result := True;
end;

function TsrdLineComment_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanCompare('//', Text, Column);
  if Result then
    Inc(Column, 2);//2 chars
end;

{ TsrdWhitespace_Scanner }

function TsrdWhitespace_Scanner.Scan(const Text: string; var Column: Integer): Boolean;
begin
  while (Column <= Length(Text)) and (Text[Column] in sWhitespace) do
    Inc(Column);
  Result := True;
end;

function TsrdWhitespace_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Text[Column] in sWhitespace;
end;

end.
