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

    procedure Push(vItem: TsrdInterpret);
    procedure Pop;
    procedure Flush; virtual;
    procedure PrepareStatement; virtual;
    procedure EndStatement; virtual;

    procedure TriggerToken(AToken: String; AType: TsrdType); virtual;
    procedure TriggerOperator(AOperator: TopOperator); virtual;
    procedure TriggerControl(AControl: TsardControl); virtual;

    property Flags: TsrdFlags read FFlags;
    property Flag: TsrdFlag read FFlag;
  end;

  TsrdInterpretClass = class of TsrdInterpret;

  { TsrdInterpretInit }

  TsrdInterpretInit = class (TsrdInterpret)
  protected
    Block: TsrdBlock;
  public
    constructor Create(AParser: TsrdParser; ABlock: TsrdBlock);
    procedure PrepareStatement; override;
    procedure TriggerControl(AControl: TsardControl); override;
  end;

  { TsrdInterpretBlock }

  TsrdInterpretBlock = class (TsrdInterpret)
  protected
    Block: TsrdBlock;
  public
  constructor Create(AParser: TsrdParser; ABlock: TsrdBlock);
    procedure PrepareStatement; override;
    procedure TriggerControl(AControl: TsardControl); override;
  end;

  { TsrdInterpretStatement }

  TsrdInterpretStatement = class (TsrdInterpret)
  public
    constructor Create(AParser: TsrdParser; AStatement: TsrdStatement);
    procedure TriggerToken(AToken: String; AType: TsrdType); override;
    procedure TriggerOperator(AOperator: TopOperator); override;
    procedure TriggerControl(AControl: TsardControl); override;
    procedure EndStatement; override;
  end;

  { TsrdInterpretParams }

  TsrdInterpretParams = class (TsrdInterpret)
  public
    procedure TriggerToken(AToken: String; AType: TsrdType); override;
    procedure TriggerOperator(AOperator: TopOperator); override;
    procedure TriggerControl(AControl: TsardControl); override;
  end;

  { TsrdInterpretDeclare }

  TsrdInterpretDeclare = class (TsrdInterpret)
  public
    procedure TriggerToken(AToken: String; AType: TsrdType); override;
    procedure TriggerOperator(AOperator: TopOperator); override;
    procedure TriggerControl(AControl: TsardControl); override;
  end;

  { TsrdParser }

  TsrdParser = class(TsardParser)
  protected
    FData: TrunData;
    function GetCurrent: TsrdInterpret;
  public
    PopItem: Boolean;
    property Current: TsrdInterpret read GetCurrent;
    procedure Push(vItem: TsrdInterpret);
    function Push(vItemClass: TsrdInterpretClass): TsrdInterpret;

    procedure Start; override;
    procedure Stop; override;
    constructor Create(AData: TrunData; ABlock: TsrdBlock);
    destructor Destroy; override;

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

{ TsrdInterpretInit }

constructor TsrdInterpretInit.Create(AParser: TsrdParser; ABlock: TsrdBlock);
begin
  inherited Create(AParser);
  Block := ABlock;
end;

procedure TsrdInterpretInit.PrepareStatement;
begin
  if Statement = nil then
  begin
    if Block = nil then
      RaiseError('Maybe you need to set a block, or it single statment block');
    Statement := Block.Add;
  end;
end;

procedure TsrdInterpretInit.TriggerControl(AControl: TsardControl);
begin
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
    else
      inherited;
  end;
end;

{ TsrdInterpretDeclare }

procedure TsrdInterpretDeclare.TriggerToken(AToken: String; AType: TsrdType);
begin
  inherited;
end;

procedure TsrdInterpretDeclare.TriggerOperator(AOperator: TopOperator);
begin
  inherited;
end;

procedure TsrdInterpretDeclare.TriggerControl(AControl: TsardControl);
begin
  inherited;
end;

{ TsrdInterpretParams }

procedure TsrdInterpretParams.TriggerToken(AToken: String; AType: TsrdType);
begin
  inherited;
end;

procedure TsrdInterpretParams.TriggerOperator(AOperator: TopOperator);
begin
  inherited;
end;

procedure TsrdInterpretParams.TriggerControl(AControl: TsardControl);
begin
  inherited;
end;

{ TsrdInterpretBlock }

procedure TsrdInterpretBlock.PrepareStatement;
begin
  if Statement = nil then
  begin
    if Block = nil then
      RaiseError('Maybe you need to set a block, or it single statment block');
    Statement := Block.Add;
  end;
end;

constructor TsrdInterpretBlock.Create(AParser: TsrdParser; ABlock: TsrdBlock);
begin
  inherited Create(AParser);
  Block := ABlock;
end;

procedure TsrdInterpretBlock.TriggerControl(AControl: TsardControl);
begin
  case AControl of
    ctlOpenBlock:
      begin
        with TsoSection.Create do
         begin
           SetObject(This);
           Push(TsrdInterpretInit.Create(Parser, Items));
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
          Push(TsrdInterpretParams.Create(Parser));
        end
        else
        if Flags = [flagIdentifier] then
        begin
          Push(TsrdInterpretBlock.Create(Parser, SetInstance.Items));
        end
        else
        with TsoStatement.Create do
        begin
          SetObject(This);
          Push(TsrdInterpretStatement.Create(Parser, Statement));
        end;
      end;
    ctlCloseParams:
      begin
        Flush;
        if Parser.Count = 1 then
          RaiseError('Maybe you closed not opened Bracket');
        Pop;
      end;
    else
      inherited;
  end;
end;

{ TsrdInterpretStatement }

constructor TsrdInterpretStatement.Create(AParser: TsrdParser; AStatement: TsrdStatement);
begin
  inherited Create(AParser);
  Statement := AStatement;
end;

procedure TsrdInterpretStatement.TriggerToken(AToken: String; AType: TsrdType);
begin
  inherited;
end;

procedure TsrdInterpretStatement.TriggerOperator(AOperator: TopOperator);
begin
  inherited;
end;

procedure TsrdInterpretStatement.TriggerControl(AControl: TsardControl);
begin
  inherited;
end;

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
  c := Column;
  while (Column <= Length(Text)) do
  begin
    if (ScanCompare('*}', Text, Column)) then
    begin
      Lexical.Parser.TriggerToken(MidStr(Text, c, Column - c), tpComment);
      Inc(Column, 2);//2 chars
      break;
    end;
    Inc(Column);
  end;
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

{ TsrdInterpret }

procedure TsrdInterpret.SetIdentifier(AIdentifier: string);
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  Identifier := AIdentifier;
  SetFlag(flagIdentifier);
end;

function TsrdInterpret.SetNumber(AIdentifier: string): TsoNumber;
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

function TsrdInterpret.SetString(AIdentifier: string): TsoString;
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

function TsrdInterpret.SetComment(AIdentifier: string): TsoComment;
begin
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

function TsrdInterpret.SetDeclare: TsoDeclare;
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

function TsrdInterpret.SetAssign: TsoAssign;
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

function TsrdInterpret.SetInstance(AIdentifier: string):TsoInstance;
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

function TsrdInterpret.SetInstance: TsoInstance;
begin
  Result := SetInstance(Identifier);
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

procedure TsrdInterpret.PrepareStatement;
begin
end;

procedure TsrdInterpret.Push(vItem: TsrdInterpret);
begin
  Parser.Push(vItem);
end;

procedure TsrdInterpret.Pop;
begin
  Parser.PopItem := True;
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
    PrepareStatement;
    Statement.Add(TokenOperator, TokenObject);
    TokenObject := nil;
    TokenOperator := nil;
  end;
end;

procedure TsrdInterpret.EndStatement;
begin
  Statement := nil;
  FFlags := [];
  FFlag := flagNone;
end;

procedure TsrdInterpret.TriggerToken(AToken: String; AType: TsrdType);
begin
  case AType of
    tpNumber: SetNumber(AToken);
    tpString: SetString(AToken);
    tpComment: SetComment(AToken);
    else
       SetIdentifier(AToken);
  end
end;

procedure TsrdInterpret.TriggerOperator(AOperator: TopOperator);
begin
  SetOperator(AOperator);
end;

procedure TsrdInterpret.TriggerControl(AControl: TsardControl);
begin
  case AControl of
    ctlOpenBlock:
      begin
        with TsoSection.Create do
         begin
           SetObject(This);
           Push(TsrdInterpretInit.Create(Parser, Items));
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
          Push(TsrdInterpretParams.Create(Parser));
        end
        else
        if Flags = [flagIdentifier] then
        begin
          Push(TsrdInterpretBlock.Create(Parser, SetInstance.Items));
        end
        else
        with TsoStatement.Create do
        begin
          SetObject(This);
          Push(TsrdInterpretStatement.Create(Parser, Statement));
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

constructor TsrdInterpret.Create(AParser: TsrdParser);
begin
  inherited Create;
  Parser := AParser;
end;

destructor TsrdInterpret.Destroy;
begin
  inherited;
end;

function TsrdParser.GetCurrent: TsrdInterpret;
begin
  Result := (inherited GetCurrent) as TsrdInterpret;
end;

procedure TsrdParser.Push(vItem: TsrdInterpret);
begin
  inherited Push(vItem);
end;

function TsrdParser.Push(vItemClass: TsrdInterpretClass): TsrdInterpret;
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
  FData := AData;
  if ABlock <> nil then
    Push(TsrdInterpretInit.Create(Self, ABlock));
end;

destructor TsrdParser.Destroy;
begin
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
  Current.TriggerControl(AControl);
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

procedure TsrdControl_Scanner.Scan(const Text: string; var Column: Integer);
var
  aControl: TctlControl;
begin
  aControl := (Lexical as TsrdLexical).Controls.Scan(Text, Column);
  if aControl <> nil then
    Column := Column + Length(aControl.Name)
  else
    RaiseError('Unkown control started with ' + Text[Column]);

  Lexical.Parser.TriggerControl(aControl.Code);
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

procedure TsrdNumber_Scanner.Scan(const Text: string; var Column: Integer);
var
  l, c: Integer;
begin
  c := Column;
  l := Length(Text);
  while (Column <= l) and (Lexical.IsNumber(Text[Column], False)) do
    Inc(Column);
  Lexical.Parser.TriggerToken(MidStr(Text, c, Column - c), tpNumber);
end;

function TsrdNumber_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexical.IsNumber(Text[Column], True);//need to improve to accept unicode chars
end;

{ TopOperatorScanner }

procedure TopOperator_Scanner.Scan(const Text: string; var Column: Integer);
var
  aOperator: TopOperator;
begin
  aOperator := (Lexical as TsrdLexical).Operators.Scan(Text, Column);
  if aOperator <> nil then
    Column := Column + Length(aOperator.Name)
  else
    RaiseError('Unkown operator started with ' + Text[Column]);

  Lexical.Parser.TriggerOperator(aOperator);
end;

function TopOperator_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexical.IsOperator(Text[Column]);
end;

{ TsrdIdentifierScanner }

procedure TsrdIdentifier_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and (Lexical.IsIdentifier(Text[Column], False)) do
    Inc(Column);
  Lexical.Parser.TriggerToken(MidStr(Text, c, Column - c), tpIdentifier);
end;

function TsrdIdentifier_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexical.IsIdentifier(Text[Column], True);
end;

{ TsrdDQStringScanner }

procedure TsrdDQString_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '"') do //TODO Escape, not now
    Inc(Column);
  Lexical.Parser.TriggerToken(MidStr(Text, c, Column - c), tpString);
  Inc(Column);
end;

function TsrdDQString_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanCompare('"', Text, Column);
  if Result then
    Inc(Column);//First char
end;

{ TsrdSQStringScanner }

procedure TsrdSQString_Scanner.Scan(const Text: string; var Column: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column <= Length(Text)) and not (Text[Column] = '''') do //TODO Escape, not now
    Inc(Column);
  Lexical.Parser.TriggerToken(MidStr(Text, c, Column - c), tpString);
  Inc(Column);
end;

function TsrdSQString_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanCompare('''', Text, Column);
  if Result then
    Inc(Column);
end;

{ TsrdBlockCommentScanner }

procedure TsrdBlockComment_Scanner.Scan(const Text: string; var Column: Integer);
begin
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
  if Result then
    Inc(Column, 2);//2 chars
end;

{ TsrdLineComment_Scanner }

procedure TsrdLineComment_Scanner.Scan(const Text: string; var Column: Integer);
begin
  while (Column <= Length(Text)) and not (Text[Column] in sEOL) do //TODO ignore quoted strings
    Inc(Column);
end;

function TsrdLineComment_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanCompare('//', Text, Column);
  if Result then
    Inc(Column, 2);//2 chars
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

end.
