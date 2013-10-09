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
  TsrdFlag = (
    flagNone,
    flagInstance,
    flagDeclare,
    flagAssign,
    flagIdentifier,
    flagConst,
    flagParam,
    flagOperator,
    flagComment,
    flagStatement,
    flagBlock
  );
  TsrdFlags = set of TsrdFlag;

  TsrdStatementType = (stmNormal, stmAssign, stmDeclare);

  TsrdParser = class;
  TsrdInterpreter = class;

  { TsrdInstruction }

  TsrdInstruction = class(TsardObject)
  protected
    procedure InternalSetObject(AObject: TsoObject);
  public
    Flag: TsrdFlag;
    Identifier: string;
    anOperator: TopOperator;
    anObject: TsoObject;
    //Return true if Identifier is not empty and object is nil
    function CheckIdentifier(vRaise: Boolean = False): Boolean;
    //Return true if Object is not nil and Identifier is empty
    function CheckObject(vRaise: Boolean = False): Boolean;
    //Return true if Operator is not nil
    function CheckOperator(vRaise: Boolean = False): Boolean;

    procedure Prepare;
    procedure SetOperator(AOperator: TopOperator);
    procedure SetIdentifier(AIdentifier: string);
    function SetNumber(AIdentifier: string): TsoNumber;
    function SetString(AIdentifier: string): TsoString;
    function SetComment(AIdentifier: string): TsoComment;
    function SetInstance(AIdentifier: string): TsoInstance;
    function SetInstance: TsoInstance;
    function SetStatment: TsoStatement;//Statement object not srdStatement
    function SetAssign: TsoAssign;
    function SetDeclare: TsoDeclare;
    procedure SetObject(AObject: TsoObject);
    procedure SetFlag(AFlag: TsrdFlag);
  end;

  { TsrdController }

  TsrdController = class(TsardObject)
  protected
    Parser: TsrdParser;
  public
    constructor Create(AParser: TsrdParser);
    procedure Control(AControl: TsardControl); virtual; abstract;
  end;

  TsrdControllerClass = class of TsrdController;

  { TsrdControllers }

  TsrdControllers = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TsrdController;
  protected
  public
    function Find(vControllerClass: TsrdControllerClass): TsrdController;
    procedure Add(AController: TsrdController);
    property Items[Index: Integer]: TsrdController read GetItem; default;
  end;

  { TsrdInterpreter }

  TsrdInterpreter = class(TsardObject)
  private
    FFlags: TsrdFlags;
  protected
    Instruction: TsrdInstruction;
    Controller: TsrdController;

    Parser: TsrdParser;
    procedure InternalPost; virtual;
  public
    constructor Create(AParser: TsrdParser);
    destructor Destroy; override;
    procedure SetFlag(AFlag: TsrdFlag);
    //Push to the Parser
    procedure Push(vItem: TsrdInterpreter);
    //No pop, but when finish Parser will pop it
    procedure Pop; virtual;
    procedure Reset; virtual;
    procedure Post; virtual;
    procedure Prepare; virtual;
    procedure Next; virtual;
    procedure AddIdentifier(AIdentifier: String; AType: TsrdType); virtual;
    procedure AddOperator(AOperator: TopOperator); virtual;

    //IsInitial check if the next object will be the first one, usefule for Assign and Declare
    function IsInitial: Boolean; virtual;

    procedure SwitchController(vControllerClass: TsrdControllerClass);
    procedure Control(AControl: TsardControl); virtual;
    property Flags: TsrdFlags read FFlags;
  end;

  TsrdInterpreterClass = class of TsrdInterpreter;

  { TsrdInterpreterStatement }

  TsrdInterpreterStatement = class (TsrdInterpreter)
  protected
    Statement: TsrdStatement;
    StatementType: TsrdStatementType;
    procedure InternalPost; override;
  public
    constructor Create(AParser: TsrdParser; AStatement: TsrdStatement);
    procedure Next; override;
    function IsInitial: Boolean; override;
  end;

  { TsrdInterpreterBlock }

  TsrdInterpreterBlock = class (TsrdInterpreterStatement)
  protected
    Block: TsrdBlock;
  public
    constructor Create(AParser: TsrdParser; ABlock: TsrdBlock);
    procedure Prepare; override;
  end;

  { TsrdInterpreterDefine }

  TsrdInterpreterDefine = class (TsrdInterpreter)
  protected
    Define: TsrdDefine;
  public
    constructor Create(AParser: TsrdParser; ADefine: TsrdDefine);
    procedure Prepare; override;
  end;

  { TsrdControllerNormal }

  TsrdControllerNormal = class(TsrdController)
  public
    procedure Control(AControl: TsardControl); override;
  end;

  { TstdControlDeclare }

  TsrdControllerDeclare = class(TsrdControllerNormal)
  public
    procedure Control(AControl: TsardControl); override;
  end;

  { TsrdControllerDeclareParams }

  TsrdControllerDeclareParams = class(TsrdControllerNormal)
  public
    procedure Control(AControl: TsardControl); override;
  end;

  { TsrdControllerAssign }

  TsrdControllerAssign = class(TsrdControllerNormal)
  public
    procedure Control(AControl: TsardControl); override;
  end;

  { TsrdParser }

  TsrdParser = class(TsardParser)
  protected
    FData: TrunData;
    function GetCurrent: TsrdInterpreter;
    procedure Created; override;
    procedure DoSetToken(AToken: String; AType: TsrdType); override;
    procedure DoSetOperator(AOperator: TsardObject); override;
    procedure DoSetControl(AControl: TsardControl); override;
  public
    Controllers: TsrdControllers;
    PopItem: Boolean;
    constructor Create(AData: TrunData; ABlock: TsrdBlock);
    destructor Destroy; override;

    property Current: TsrdInterpreter read GetCurrent;
    procedure Push(vItem: TsrdInterpreter);
    function Push(vItemClass: TsrdInterpreterClass): TsrdInterpreter;

    procedure Start; override;
    procedure Stop; override;

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

{ TsrdInterpreterDefine }

constructor TsrdInterpreterDefine.Create(AParser: TsrdParser; ADefine: TsrdDefine);
begin
  inherited Create(AParser);
  Define := ADefine;
end;

procedure TsrdInterpreterDefine.Prepare;
begin
  inherited Prepare;
end;

{ TsrdControllerDeclareParams }

procedure TsrdControllerDeclareParams.Control(AControl: TsardControl);
begin
  inherited Control(AControl);
end;

{ TsrdInstruction }

procedure TsrdInstruction.InternalSetObject(AObject: TsoObject);
begin
  if (anObject <> nil) and (AObject <> nil) then
    RaiseError('Object is already set');
  anObject := AObject;
end;

function TsrdInstruction.CheckIdentifier(vRaise: Boolean): Boolean;
begin
  Result := Identifier <> '';
  if vRaise and not Result then
    RaiseError('Identifier is not set!');
  Result := Result and (anObject = nil);
  if vRaise and not Result then
    RaiseError('Object is already set!');
end;

function TsrdInstruction.CheckObject(vRaise: Boolean): Boolean;
begin
  Result := anObject <> nil;
  if vRaise and not Result then
    RaiseError('Object is not set!');
  Result := Result and (Identifier = '');
  if vRaise and not Result then
    RaiseError('Identifier is already set!');
end;

function TsrdInstruction.CheckOperator(vRaise: Boolean): Boolean;
begin
  Result := anOperator <> nil;
  if vRaise and not Result then
    RaiseError('Operator is not set!');
end;

procedure TsrdInstruction.Prepare;
begin
  if Identifier <> '' then
  begin
    SetInstance(Identifier);
    Identifier := '';
  end;
end;

{ TstdControlDeclare }

procedure TsrdControllerDeclare.Control(AControl: TsardControl);
begin
  case AControl of
    ctlOpenParams:
      begin
      end;
    ctlCloseParams:
      begin
      end;
    else
      inherited
  end;
end;

{ TstdControlAssign }

procedure TsrdControllerAssign.Control(AControl: TsardControl);
begin
  inherited;
{  with Parser.Current do
    case AControl of
    end;}
end;

{ TsrdControllerNormal }

procedure TsrdControllerNormal.Control(AControl: TsardControl);
var
  stm: TsrdStatement;
begin
  with Parser.Current do
  case AControl of
    ctlAssign:
      begin
        if IsInitial then
        begin
          Instruction.SetAssign;
          Post;
        end
        else
          RaiseError('You can not use assignment here!');
      end;
    ctlDeclare:
      begin
        if IsInitial then
        begin
          stm := Instruction.SetDeclare.Statement;
          Post;
          //Push(TsrdInterpreterDefine.);
          //Statement := stm;//<-- TODO: wrong wrong
          SwitchController(TsrdControllerDeclare);
        end
        else
          RaiseError('You can not use assignment here!');
      end;

    ctlOpenBlock:
      begin
        with TsoSection.Create do
         begin
           Instruction.SetObject(This);
           Push(TsrdInterpreterBlock.Create(Parser, Block));
         end;
      end;
    ctlCloseBlock:
      begin
        Post;
        if Parser.Count = 1 then
          RaiseError('Maybe you closed not opened Curly');
        Pop;
      end;
    ctlOpenParams:
      begin
        //Params of function or object like: Sin(10)
        if Instruction.CheckIdentifier then
        begin
          with Instruction.SetInstance do
            Push(TsrdInterpreterBlock.Create(Parser, Block));
        end
        else //No it is just sub statment like: 10+(5*5)
        with Instruction.SetStatment do
        begin
          Push(TsrdInterpreterStatement.Create(Parser, Statement));
        end;
      end;
    ctlCloseParams:
      begin
        Post;
        if Parser.Count = 1 then
          RaiseError('Maybe you closed not opened Bracket');
        Pop;
      end;
    ctlStart:
      begin
      end;
    ctlStop:
      begin
        Post;
      end;
    ctlEnd:
      begin
        Post;
        Next;
      end;
    ctlNext:
      begin
        Post;
        Next;
      end;
    else
      RaiseError('Not implemented yet, sorry :(');
  end;
end;

{ TsrdControllers }

function TsrdControllers.GetItem(Index: Integer): TsrdController;
begin
  Result := inherited Items[Index] as TsrdController;
end;

function TsrdControllers.Find(vControllerClass: TsrdControllerClass): TsrdController;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if vControllerClass = Items[i].ClassType then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure TsrdControllers.Add(AController: TsrdController);
begin
  inherited Add(AController);
end;

{ TsrdController }

constructor TsrdController.Create(AParser: TsrdParser);
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

{ TsrdInterpreterBlock }

procedure TsrdInterpreterBlock.Prepare;
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
  inherited Create(AParser, nil);
  Block := ABlock;
end;

{ TsrdInterpreterStatement }

procedure TsrdInterpreterStatement.InternalPost;
begin
  inherited;
  Statement.Add(Instruction.anOperator, Instruction.anObject);
end;

constructor TsrdInterpreterStatement.Create(AParser: TsrdParser; AStatement: TsrdStatement);
begin
  inherited Create(AParser);
  Statement := AStatement;
end;

procedure TsrdInterpreterStatement.Next;
begin
  Statement := nil;
  inherited;
end;

function TsrdInterpreterStatement.IsInitial: Boolean;
begin
  Result := (Statement = nil) or (Statement.Count = 0);
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
      Lexical.Parser.SetToken(Buffer, tpComment);
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
  Lexical.Parser.SetControl(ctlStart);
end;

procedure TsrdFeeder.DoStop;
begin
  inherited;
  Lexical.Parser.SetControl(ctlStop);
end;

{ TsrdInstruction. }

procedure TsrdInstruction.SetIdentifier(AIdentifier: string);
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  Identifier := AIdentifier;
  SetFlag(flagIdentifier);
end;

function TsrdInstruction.SetNumber(AIdentifier: string): TsoNumber;
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  //TODO need to check anObject too
  if (pos('.', AIdentifier) > 0) or ((pos('E', AIdentifier) > 0)) then
    Result := TsoFloat.Create(StrToFloat(AIdentifier))
  else
    Result := TsoInteger.Create(StrToInt64(AIdentifier));
  InternalSetObject(Result);
  SetFlag(flagConst);
end;

function TsrdInstruction.SetString(AIdentifier: string): TsoString;
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  //TODO need to check anObject too
  Result := TsoString.Create;
  with Result do
  begin
    Value := AIdentifier;
  end;
  InternalSetObject(Result);
  SetFlag(flagConst);
end;

function TsrdInstruction.SetComment(AIdentifier: string): TsoComment;
begin
  //We need to check if it the first expr in the statment
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  //TODO need to check anObject too
  Result := TsoComment.Create;
  with Result do
  begin
    Value := AIdentifier;
  end;
  InternalSetObject(Result);
  SetFlag(flagComment);
end;

function TsrdInstruction.SetDeclare: TsoDeclare;
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  Result := TsoDeclare.Create;
  with Result do
  begin
    Name := Identifier;
//    ID := Parser.Data.RegisterID(Name);
  end;
  InternalSetObject(Result);
  Identifier := '';
  SetFlag(flagDeclare);
end;

function TsrdInstruction.SetAssign: TsoAssign;
begin
  //Do not check the Identifier if empty, becuase it is can be empty to assign to result of block
  Result := TsoAssign.Create;
  with Result do
  begin
    Name := Identifier;
    //ID := Parser.Data.RegisterID(Name);
  end;
  InternalSetObject(Result);
  Identifier := '';
  SetFlag(flagAssign);
end;

function TsrdInstruction.SetInstance(AIdentifier: string):TsoInstance;
begin
  if AIdentifier <> '' then
    RaiseError('Identifier not set');
  Result := TsoInstance.Create;
  with Result do
  begin
    Name := AIdentifier;
    //ID := Parser.Data.RegisterID(Name);
  end;
  InternalSetObject(Result);
  SetFlag(flagInstance);
end;

function TsrdInstruction.SetInstance: TsoInstance;
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  Result := SetInstance(Identifier);
  Identifier := '';
end;

function TsrdInstruction.SetStatment: TsoStatement;
begin
  if Identifier <> '' then
    RaiseError('Identifier is set');
  Result := TsoStatement.Create;
  with Result do
  begin
    //ID := Parser.Data.RegisterID(Name);
  end;
  InternalSetObject(Result);
  SetFlag(flagStatement);
end;

procedure TsrdInstruction.SetOperator(AOperator: TopOperator);
begin
  if anOperator <> nil then
    RaiseError('Operator is already set');
  anOperator := AOperator;
end;

procedure TsrdInstruction.SetObject(AObject: TsoObject);
begin
  if Identifier <> '' then
    RaiseError('Identifier is already set');
  InternalSetObject(AObject);
end;

procedure TsrdInstruction.SetFlag(AFlag: TsrdFlag);
begin
  Flag := AFlag;
end;

{ TsrdInterpreter }

procedure TsrdInterpreter.SetFlag(AFlag: TsrdFlag);
begin
  FFlags := FFlags + [AFlag];
end;

procedure TsrdInterpreter.Prepare;
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

procedure TsrdInterpreter.Reset;
begin
  FreeAndNil(Instruction);
  Instruction := TsrdInstruction.Create;
end;

procedure TsrdInterpreter.Post;
begin
  Instruction.Prepare;
  if (Instruction.anObject <> nil) or (Instruction.anOperator <> nil) then
  begin
    if Instruction.Identifier <> '' then
      RaiseError('Identifier is already set, can not Post');
    if Instruction.anObject = nil then
      RaiseError('Object is nil!');
    Prepare;
    InternalPost;
    Reset;
  end;
end;

procedure TsrdInterpreter.Next;
begin
  FFlags := [];
end;

procedure TsrdInterpreter.AddIdentifier(AIdentifier: String; AType: TsrdType);
begin
  case AType of
    tpNumber: Instruction.SetNumber(AIdentifier);
    tpString: Instruction.SetString(AIdentifier);
    tpComment: Instruction.SetComment(AIdentifier);
    else
       Instruction.SetIdentifier(AIdentifier);
  end
end;

procedure TsrdInterpreter.AddOperator(AOperator: TopOperator);
begin
  Post;
  Instruction.SetOperator(AOperator);
end;

function TsrdInterpreter.IsInitial: Boolean;
begin
  Result := False;
end;

procedure TsrdInterpreter.SwitchController(vControllerClass: TsrdControllerClass);
begin
  Controller := Parser.Controllers.Find(vControllerClass);
end;

procedure TsrdInterpreter.Control(AControl: TsardControl);
begin
  Controller.Control(AControl);
end;

procedure TsrdInterpreter.InternalPost;
begin

end;

constructor TsrdInterpreter.Create(AParser: TsrdParser);
begin
  inherited Create;
  Parser := AParser;
  SwitchController(TsrdControllerNormal);
  Reset;
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
end;

procedure TsrdParser.Push(vItem: TsrdInterpreter);
begin
  inherited Push(vItem);
end;

function TsrdParser.Push(vItemClass: TsrdInterpreterClass): TsrdInterpreter;
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
  if ABlock = nil then
    RaiseError('You must set a block');
  Controllers := TsrdControllers.Create;
  Controllers.Add(TsrdControllerNormal.Create(Self));
  Controllers.Add(TsrdControllerAssign.Create(Self));
  Controllers.Add(TsrdControllerDeclare.Create(Self));
  FData := AData;
  Push(TsrdInterpreterBlock.Create(Self, ABlock));
end;

destructor TsrdParser.Destroy;
begin
  FreeAndNil(Controllers);
  inherited;
end;

procedure TsrdParser.DoSetToken(AToken: String; AType: TsrdType);
begin
  Current.AddIdentifier(AToken, AType);
  if PopItem then
  begin
    PopItem := False;
    Pop;
  end;
end;

procedure TsrdParser.DoSetControl(AControl: TsardControl);
begin
  Current.Control(AControl);
  if PopItem then
  begin
    PopItem := False;
    Pop;
  end;
end;

procedure TsrdParser.DoSetOperator(AOperator: TsardObject);
begin
  Current.AddOperator(AOperator as TopOperator);
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

  Lexical.Parser.SetControl(aControl.Code);
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
  Lexical.Parser.SetToken(MidStr(Text, c, Column - c), tpNumber);
  Result := True;
end;

function TsrdNumber_Scanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexical.IsNumber(Text[Column], True);//need to improve to accept unicode chars
end;

{ TopOperatorScanner }

function TopOperator_Scanner.Scan(const Text: string; var Column: Integer): Boolean;
var
  lOperator: TopOperator;
begin
  lOperator := (Lexical as TsrdLexical).Operators.Scan(Text, Column);
  if lOperator <> nil then
    Column := Column + Length(lOperator.Name)
  else
    RaiseError('Unkown operator started with ' + Text[Column]);

  Lexical.Parser.SetOperator(lOperator);
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
  Lexical.Parser.SetToken(MidStr(Text, c, Column - c), tpIdentifier);
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
      Lexical.Parser.SetToken(Buffer, tpString);
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
