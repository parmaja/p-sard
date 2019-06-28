unit sardScripts;
{**
*  This file is part of the "SARD"
*
* @license   The MIT License (MIT)
*            Included in this distribution
* @author    Zaher Dirkey <zaher at parmaja dot com>
*}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils,
  sardClasses, sardObjects, sardParsers, sardStandards;

  //sColorOpenChars = ['#'];
  //sColorChars = sColorOpenChars + ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];

type
  { TCodeLexer }

  TCodeLexer = class(TLexer)
  protected
    const
      sWhitespace = sEOL + [' ', #8];
      sNumberOpenChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
      sNumberChars = sNumberOpenChars + ['.', 'x', 'h', 'a', 'b', 'c', 'd', 'e', 'f'];
      sSymbolChars = ['"', '''', '\']; //deprecated;
      sIdentifierSeparator = '.';
  public
    constructor Create; override;
    function IsEOL(vChar: Char): Boolean; override;
    function IsWhiteSpace(vChar: char; vOpen: Boolean =true): Boolean; override;
    function IsControl(vChar: Char): Boolean; override;
    function IsOperator(vChar: Char): Boolean; override;
    function IsNumber(vChar: Char; vOpen: Boolean =true): Boolean; override;
    function IsSymbol(vChar: Char): Boolean; override;
    function IsIdentifier(vChar: Char; vOpen: Boolean =true): Boolean;
  end;

  TCodeParser = class;

  { TInstruction }

  TInstruction = record
  public
    procedure internalSetObject(aObject: TNode);
  public
    Identifier: string;
    AnOperator: TSardOperator;
    AnObject: TNode;
    //Return true if Identifier is not empty and object is nil
    function CheckIdentifier(raiseIt: Boolean = false): Boolean;
    //Return true if Object is not nil and Identifier is empty
    function CheckObject(raiseIt: Boolean = false): Boolean;
    //Return true if Operator is not nil
    function CheckOperator(raiseIt: Boolean = false): Boolean;

    function IsEmpty: Boolean;
    procedure SetOperator(AOperator: TSardOperator);
    procedure SetIdentifier(AIdentifier: string);
    function SetNumber(AIdentifier: string): TNumber_Node;
    function SetText(Text: string): TText_Node;
    function SetComment(AIdentifier: string): TComment_Node;
    procedure SetObject(AObject: TNode);
    function SetInstance(AIdentifier: string): TInstance_Node; overload;
    function SetInstance: TInstance_Node; overload;
    function SetEnclose: TEnclose_Node;
    function SetAssign: TAssign_Node;
    function SetDeclare: TDeclare_Node;
  end;

  { TControllerNormal }

  TControllerNormal = class(TController)
  protected
  public
    procedure SetControl(Control: TSardControl); override;
  end;

  { TControllerDefines }

  TControllerDefines = class(TController)
  public
    procedure SetControl(AControl: TSardControl); override;
  end;

  { TCodeCollector }

  TCodeCollector = class(TCollector)
  private
    FInstruction: TInstruction;
  public
    procedure Reset; override;
    procedure Prepare; override;
    procedure Post; override;
    procedure Next; override;
    procedure AddToken(Token: TSardToken); override;
    procedure AddOperator(AOperator: TSardOperator); virtual;

    property Instruction: TInstruction read FInstruction;
  end;

  { TCollectorStatement }

  TCollectorStatement = class(TCodeCollector)
  private
    FStatement: TStatement;
  protected
    function CreateController: TController; override;
    procedure InternalPost; override;
    property Statement: TStatement read FStatement;
  public
    constructor Create(AParser: TParser; AStatement: TStatement);
    procedure Prepare; override;
    function IsInitial: Boolean; override;
    procedure Next; override;
  end;

  { TCollectorBlock }

  TCollectorBlock = class(TCollectorStatement)
  private
    FStatements: TStatements;
  protected
  public
    constructor Create(AParser: TParser; AStatements: TStatements);
    procedure Prepare; override;
    property Statements: TStatements read FStatements;
  end;

  {
      Define is a parameters defines in declare

      //parameters are in the declaration, arguments are the things actually passed to it. so void f(x), f(0), x is the parameter, 0 is the argument
  }

  { TCollectorDeclare }

  TCollectorDeclare = class(TCollectorStatement)
  private
  protected
  public
    constructor Create(AParser: TParser);
    procedure AddControl(AControl: TSardControl); override;
  end;

  { TCollectorDefine }

  TCollectorDefine = class(TCodeCollector)
  protected
    type
      TState = (stName, stType);
  protected
    State: TState;
    Param: Boolean;
    Declare: TDeclare_Node;
    procedure InternalPost; override;
    function CreateController: TController; override;
  public
    constructor Create(AParser: TParser; ADeclare: TDeclare_Node);
    {
      x:int  (p1: int; p2: string);
      ^type (-------Params------)^
      Declare  ^Declare
      We end with ; or : or )
    }
    procedure AddControl(AControl: TSardControl); override;
    procedure Reset; override;
    function IsInitial: Boolean; override;
  end;

  { TCodeParser }

  TCodeParser = class(TParser)
  protected
    LastControl: TSardControlID;
    Lexer: TLexer;
    procedure DoQueue;
  public
    constructor Create(ALexer: TLexer; AStatements: TStatements);
    destructor Destroy; override;

    function IsKeyword(AIdentifier: string): Boolean; override;
    procedure SetToken(Token: TSardToken); override;
    procedure SetOperator(AOperator: TSardOperator); override;
    procedure SetControl(AControl: TSardControl); override;
    procedure SetWhiteSpaces(Whitespaces: string); override;
    procedure AfterPush; override;
    procedure BeforePop; override;
    procedure Start; override;
    procedure Stop; override;
  end;

  { TCodeScanner }

  TCodeScanner = class(TScanner)
  protected
    Block: TBlock_Node;
    Parser: TCodeParser;
    function CreateParser: TParser; override;
  public
    constructor Create(ABlock: TBlock_Node);
  end;

  { TVersion_Const_Node }

  TVersion_Const_Node = class(TNode)
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
  end;

  { TPI_Const_Node }

  TPI_Const_Node = class(TNode)
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
  end;

  { TTime_Const_Node }

  TTime_Const_Node = class(TNode)
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
  end;

  { TPrint_Object_Node }

  TPrint_Object_Node = class(TNode)
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
  end;

  { TSardScript }

  TCodeScript = class(TScript)
  public
    Main: TBlock_Node;
    Scanner: TScanner;
    Result: string;
    destructor Destroy; override;
    procedure Compile(Lines: TStringList); override; overload;
    procedure Run;
  end;

implementation

{ TCodeCollector }

procedure TCodeCollector.Reset;
begin
  //destroy(instruction);
  FInstruction := Default(TInstruction);
  //instruction= new Instruction;
end;

procedure TCodeCollector.Prepare;
begin

end;

procedure TCodeCollector.Post;
begin
  if (instruction.IsEmpty) then
  begin
      //debug(log_compile) writeln("Post() empty");
  end
  else
  begin
      Prepare;
      InternalPost;
  end;
  Reset;
end;

procedure TCodeCollector.Next;
begin
end;

procedure TCodeCollector.AddToken(Token: TSardToken);
var
  text: string;
begin
  text := Token.Value;
  case Token.TokenType of
      typeNumber:
        Instruction.SetNumber(text);
      typeString:
        Instruction.SetText(text);
      typeEscape:
      begin
          //TODO text = //need function doing escapes
          if (text = '\n') then
            text := #13
          else if (text = '\r') then
            text := #10
          else if (text = '\"') then
            text := '"'
          else if (text = '\''') then
            text := ''''
          else if (text = '\\') then
            text := '\';
          Instruction.setText(text);
      end;
      typeComment:
        instruction.SetComment(text);
      else
        Instruction.SetIdentifier(text);
  end
end;

procedure TCodeCollector.AddOperator(AOperator: TSardOperator);
begin
  Post;
  Instruction.SetOperator(AOperator);
end;

{ TCodeLexer }

constructor TCodeLexer.Create;
begin
  inherited;
  with Symbols do
  begin
  end;

  with Controls do
  begin
    Add('', ctlNone);////TODO i feel it is so bad
    Add('', ctlToken);
    Add('', ctlOperator);
    Add('', ctlStart);
    Add('', ctlStop);
    //Add('', ctlDeclare);
    //Add('', ctlAssign);

    Add('(', ctlOpenParams);
    Add('[', ctlOpenArray);
    Add('{', ctlOpenBlock);
    Add(')', ctlCloseParams);
    Add(']', ctlCloseArray);
    Add('}', ctlCloseBlock);
    Add(';', ctlEnd);
    Add(',', ctlNext);
    Add(':', ctlDeclare);
    Add(':=', ctlAssign);
  end;

  with Operators do
  begin
    Add(TOpPlus.Create);
    Add(TOpSub.Create);
    Add(TOpMultiply.Create);
    Add(TOpDivide.Create);

    Add(TOpEqual.Create);
    Add(TOpNotEqual.Create);
    Add(TOpAnd.Create);
    Add(TOpOr.Create);
    Add(TOpNot.Create);

    Add(TOpGreater.Create);
    Add(TOpLesser.Create);

    Add(TOpPower.Create);
  end;

  with (Self) do
  begin
      Add(TWhitespace_Tokenizer.Create);
      Add(TBlockComment_Tokenizer.Create);
      Add(TComment_Tokenizer.Create);
      Add(TLineComment_Tokenizer.Create);
      Add(TNumber_Tokenizer.Create);
      Add(TSQString_Tokenizer.Create);
      Add(TDQString_Tokenizer.Create);
      Add(TEscape_Tokenizer.Create);
      Add(TControl_Tokenizer.Create);
      Add(TOperator_Tokenizer.Create); //Register it after comment because comment take /*
      Add(TIdentifier_Tokenizer.Create);//Sould be last one
  end;
end;

function TCodeLexer.IsEOL(vChar: Char): Boolean;
begin
  Result := CharInSet(vChar, sEOL);
end;

function TCodeLexer.IsWhiteSpace(vChar: char; vOpen: Boolean): Boolean;
begin
  Result := CharInSet(vChar, sWhitespace);
end;

function TCodeLexer.IsControl(vChar: Char): Boolean;
begin
  Result := Controls.IsOpenBy(vChar);
end;

function TCodeLexer.IsOperator(vChar: Char): Boolean;
begin
  Result := Operators.IsOpenBy(vChar);
end;

function TCodeLexer.IsNumber(vChar: Char; vOpen: Boolean): Boolean;
begin
  if (vOpen) then
    Result := CharInSet(vChar, sNumberOpenChars)
  else
    Result := CharInSet(vChar, sNumberChars);
end;

function TCodeLexer.IsSymbol(vChar: Char): Boolean;
begin
  Result := CharInSet(vChar, sSymbolChars) or Symbols.IsOpenBy(vChar);
end;

function TCodeLexer.IsIdentifier(vChar: Char; vOpen: Boolean): Boolean;
begin
  Result := inherited isIdentifier(vChar, vOpen); //we do not need to override it, but it is nice to see it here
end;

{ TCodeParser }

function TCodeParser.IsKeyword(AIdentifier: string): Boolean;
begin
  //example just for fun
  {
  if (Identifier = "begin") then
  begin
      SetControl(ctlOpenBlock);
      Result := true;
  end
  if (Identifier = "end") then
  begin
      SetControl(ctlCloseBlock);
      Result := true;
  end;
  else  }
  Result := False;
end;

procedure TCodeParser.SetToken(Token: TSardToken);
begin
  //here is the magic, we must find it in tokens detector to check if this id is normal id or is control or operator
  //by default it is id
  if (Token.TokenType <> typeIdentifier) or (not IsKeyword(Token.Value)) then
  begin
    (*
        We will send ; after } if we find a token
            x:= {
                    ...
                } <---------here not need to add ;
            y := 10;
    *)
    if (LastControl = ctlCloseBlock) then
    begin
      LastControl := ctlNone;//prevent loop
      SetControl(Lexer.Controls.GetControl(ctlEnd));
    end;
    Current.AddToken(Token);
    DoQueue();
    FActions := [];
    LastControl := ctlToken;
  end;
end;

procedure TCodeParser.SetOperator(AOperator: TSardOperator);
var
  o: TSardOperator;
begin
  inherited;
  o := AOperator;
  if (o = nil) then
    RaiseError('SetOperator not Operator');
  (Current as TCodeCollector).AddOperator(o);
  DoQueue();
  FActions := [];
  LastControl := ctlOperator;
end;

procedure TCodeParser.SetControl(AControl: TSardControl);
begin
  inherited;
  if (LastControl = ctlCloseBlock) then //see setToken
  begin
      LastControl := ctlNone;//prevent loop
      SetControl(Lexer.Controls.GetControl(ctlEnd));
  end;

  Current.AddControl(AControl);
  DoQueue();
  if (paBypass in Actions) then //TODO check if Set work good here
      Current.AddControl(AControl);
  FActions := [];
  LastControl := aControl.Code;
end;

procedure TCodeParser.SetWhiteSpaces(Whitespaces: string);
begin
  inherited;
  //nothing to do
end;

procedure TCodeParser.AfterPush;
begin
  inherited;
end;

procedure TCodeParser.BeforePop;
begin
  inherited;
end;

procedure TCodeParser.DoQueue;
begin
  if (paPop in actions) then
  begin
      FActions := FActions - [paPop];
      Pop();
  end;

  if (NextCollector <> nil) then
  begin
      Push(NextCollector);
      FNextCollector := nil;
  end
end;

constructor TCodeParser.Create(ALexer: TLexer; AStatements: TStatements);
begin
  inherited Create;
  Lexer := ALexer;
  if (AStatements = nil) then
    RaiseError('You should set Parser.Statements!');
  Push(TCollectorBlock.Create(Self, AStatements));
end;

destructor TCodeParser.Destroy;
begin
  Pop;
  inherited;
end;

procedure TCodeParser.Start;
begin
  inherited;
  SetControl(Lexer.Controls.GetControl(ctlStart));
end;

procedure TCodeParser.Stop;
begin
  inherited;
  SetControl(Lexer.Controls.GetControl(ctlStop));
end;

{ TCodeScanner }

function TCodeScanner.CreateParser: TParser;
begin
  Result := TCodeParser.Create(Lexer, Block.Statements);
end;

constructor TCodeScanner.Create(ABlock: TBlock_Node);
begin
  inherited Create;
  Block := ABlock;
  Add(TCodeLexer.Create);
end;

{ TSardScript }

destructor TCodeScript.Destroy;
begin
  FreeAndNil(Main);
  FreeAndNil(Scanner);
  inherited Destroy;
end;

procedure TCodeScript.Compile(Lines: TStringList);
var
  Version_Const: TVersion_Const_Node;
  PI_Const: TPI_Const_Node;
  Print_Object: TPrint_object_Node;
begin
  //writeln("-------------------------------");

  FreeAndNil(Main);

  Main := TBlock_Node.Create; //destory the old compile and create new
//  Main.Name := 'main';

  Version_Const := TVersion_Const_Node.Create;
  Version_Const.Name := 'Version';
  Main.DeclareObject(Version_Const);

  PI_const := TPI_Const_Node.Create();
  PI_Const.name := 'PI';
  Main.DeclareObject(PI_Const);

  Print_Object := TPrint_object_Node.Create();
  Print_Object.Name := 'print';
  with Main.DeclareObject(Print_Object) do
    Defines.Parameters.Add('s', 'string');

  // Compile
  Scanner := TCodeScanner.Create(Main);
  Scanner.Scan(Lines);
end;

procedure TCodeScript.Run;
var
  env: TRunEnv;
begin
  env := TRunEnv.Create;
  try
    Env.Results.Push;
    //Env.Root.AnObject := Main;
    Main.Execute(Env.Root, Env, nil);

    if (Env.Results.Current <> nil) and (Env.Results.Current.Result.Value <> nil) then
    begin
      Result := Env.Results.Current.Result.Value.AsText;
    end;
    Env.Results.Pop;
  finally
    FreeAndNil(Env);
  end;
end;

{ TPrint_Object_Node }

procedure TPrint_Object_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
var
  v: TRunValue;
begin
  v := Env.Stack.Current.Variables.Find('s');
  if (v <> nil) and (v.Value <> nil) then
  begin
    WriteLn(V.Value.AsText);
    Done := true;
  end;
end;

{ TTime_Const_Node }

procedure TTime_Const_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
begin
  Env.Results.Current.Result.Value := TReal_Node.Create(Now);
  Done := True;
end;

{ TPI_Const_Node }

procedure TPI_Const_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
begin
  Env.Results.Current.Result.Value := TReal_Node.Create(Pi);
  Done := True;
end;

{ TVersion_Const_Node }

procedure TVersion_Const_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
begin
  Env.Results.Current.Result.Value := TText_Node.Create(sSardVersion);
  Done := True;
end;

{ TControllerNormal }

procedure TControllerNormal.SetControl(Control: TSardControl);
var
  aDeclare: TDeclare_Node;
  aBlock: TBlock_Node;
begin
  with (collector as TCodeCollector) do
  begin
    case Control.Code of
      ctlAssign:
      begin
        if (IsInitial) then
        begin
          Instruction.SetAssign();
          Post;
        end
        else
          RaiseError('You can not use assignment here!');
      end;

      ctlDeclare:
      begin
        if (IsInitial) then
        begin
            aDeclare := Instruction.SetDeclare;
            Post;
            Parser.Push(TCollectorDefine.Create(Parser, aDeclare));
        end
        else
            RaiseError('You can not use a declare here!');
      end;
      ctlOpenBlock:
      begin
        aBlock := TBlock_Node.Create();
        Instruction.SetObject(aBlock);
        Parser.Push(TCollectorBlock.Create(Parser, aBlock.statements));
      end;

      ctlCloseBlock:
      begin
        Post;
        if (Parser.Count = 1) then
          RaiseError('Maybe you closed not opened Curly');
        Parser.SetAction([paPop]);
      end;

      ctlOpenParams:
      begin
        //params of function/object like: Sin(10)
        if Instruction.CheckIdentifier then
        begin
          with Instruction.SetInstance do
            Parser.Push(TCollectorBlock.Create(Parser, arguments));
        end
        else //No it is just sub statment like: 10+(5*5)
          with Instruction.SetEnclose do
            Parser.Push(TCollectorStatement.Create(Parser, statement));
      end;

      ctlCloseParams:
      begin
          Post;
          if (Parser.Count = 1) then
            RaiseError('Maybe you closed not opened Bracket');
          Parser.setAction([paPop]);
      end;

      ctlStart:
      begin
      end;

      ctlStop:
        Post;
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
        RaiseError('Not implemented yet :(');
    end;
  end;
end;

{ TCollectorDefine }

constructor TCollectorDefine.Create(AParser: TParser; ADeclare: TDeclare_Node);
begin
  inherited Create(AParser);
  Declare := ADeclare;
end;

procedure TCollectorDefine.InternalPost;
begin
  inherited;
  if (Instruction.Identifier = '') then
    RaiseError('Identifier not set'); //TODO maybe check if he Post const or another things
  if (Param) then
  begin
    if (State = stName) then
      Declare.Defines.Parameters.Add(Instruction.Identifier, '')
    else
    begin
      if (Declare.Defines.Parameters.Last.DefType <> '') then
        RaiseError('Result type already set');
      Declare.Defines.Parameters.Last.DefType := Instruction.Identifier;
    end;
  end
  else
    Declare.ResultType := Instruction.Identifier;
end;

function TCollectorDefine.CreateController: TController;
begin
  Result := TControllerDefines.Create(Self);
end;

procedure TCollectorDefine.AddControl(AControl: TSardControl);
var
  aBlock: TBlock_Node;
begin
  with Parser do
  begin
    case AControl.Code of
      ctlOpenBlock:
      begin
          Post;
          aBlock := TBlock_Node.Create;
          aBlock.Parent := Declare;
          Declare.ExecuteObject := aBlock;
          //We will pass the control to the next Collector
          SetAction([paPop], TCollectorBlock.Create(Parser, aBlock.Statements));
      end;

      ctlDeclare:
      begin
          if (param) then
          begin
            Post;
            State := stType;
          end
          else
          begin
            Post;
            SetAction([paPop]);
          end;
      end;

      ctlAssign:
      begin
          Post;
          Declare.ExecuteObject := TAssign_Node.Create;
          Declare.ExecuteObject.Parent := Declare;
          Declare.ExecuteObject.Name := Declare.Name;
          SetAction([paPop]); //Finish it, mean there is no body/statment for the declare
      end;

      ctlEnd:
      begin
          if (param) then
          begin
            Post;
            State := stName;
          end
          else
          begin
            Post;
            SetAction([paPop]);
          end;
      end;

      ctlNext:
      begin
        Post;
        State := stName;
      end;

      ctlOpenParams:
      begin
          Post;
          if (declare.defines.parameters.count > 0) then
            RaiseError('You already define params! we expected open block.');
          Param := true;
      end;

      ctlCloseParams:
      begin
          Post;
          //pop(); //Finish it
          Param := False;
          //action(paPop), TCollectorBlock.Create(Parser, declare.block)); //return to the statment
      end;

      else
          inherited;
    end;
  end;
  inherited;
end;

procedure TCollectorDefine.Reset;
begin
  State := stName;
  inherited;
end;

function TCollectorDefine.IsInitial: Boolean;
begin
  Result := true;
end;

{ TCollectorDeclare }

constructor TCollectorDeclare.Create(AParser: TParser);
begin
  inherited Create(AParser, nil);
end;

procedure TCollectorDeclare.AddControl(AControl: TSardControl);
begin
  case (AControl.code) of
    ctlEnd, ctlNext:
    begin
      Post;
      Parser.SetAction([paPop, paBypass]);
    end
    else
      inherited;
  end;
end;

{ TCollectorBlock }

constructor TCollectorBlock.Create(AParser: TParser; AStatements: TStatements);
begin
  inherited Create(AParser, nil);
  FStatements := AStatements;
end;

procedure TCollectorBlock.Prepare;
begin
  inherited;
  if (Statement = nil) then
  begin
    if (Statements = nil) then
      RaiseError('Maybe you need to set a block, or it single statment block');
    FStatement := Statements.Add;
  end;
end;

{ TCollectorStatement }

function TCollectorStatement.CreateController: TController;
begin
  Result := TControllerNormal.Create(Self);
end;

procedure TCollectorStatement.InternalPost;
begin
  inherited;
  Statement.Add(Instruction.AnOperator, Instruction.AnObject);
end;

constructor TCollectorStatement.Create(AParser: TParser; AStatement: TStatement);
begin
  inherited Create(AParser);
  FStatement := AStatement;
end;

procedure TCollectorStatement.Prepare;
begin
  inherited;
  if (Instruction.identifier <> '') then
  begin
    if (Instruction.AnObject <> nil) then
      RaiseError('Object is already set!');
    Instruction.SetInstance;
  end;
end;

function TCollectorStatement.IsInitial: Boolean;
begin
  Result := (Statement = nil) or (Statement.Count = 0);
end;

procedure TCollectorStatement.Next;
begin
  inherited;
  FStatement := nil;
end;

{ TControllerDefines }

procedure TControllerDefines.SetControl(AControl: TSardControl);
begin
  //nothing O.o
  //TODO change the inheretance
end;

{ TInstruction }

procedure TInstruction.internalSetObject(aObject: TNode);
begin
  if ((AnObject <> nil) and (aObject <> nil)) then
      RaiseError('Object is already set');
  AnObject := aObject;
end;

function TInstruction.CheckIdentifier(raiseIt: Boolean): Boolean;
begin
  Result := Identifier <> '';
  if RaiseIt and not Result then
    RaiseError('Identifier is not set!');
  Result := Result and (AnObject = nil);
  if RaiseIt and not Result then
    RaiseError('Object is already set!');
end;

function TInstruction.CheckObject(raiseIt: Boolean): Boolean;
begin
  Result := AnObject <> nil;
  if RaiseIt and not Result then
    RaiseError('Object is not set!');
  Result := Result and (Identifier = '');
  if RaiseIt and not Result then
    RaiseError('Identifier is already set!');
end;

function TInstruction.CheckOperator(raiseIt: Boolean): Boolean;
begin
  Result := AnOperator <> nil;
  if RaiseIt and not Result then
    RaiseError('Operator is not set!');
end;

function TInstruction.IsEmpty: Boolean;
begin
  Result := not((Identifier <> '') or (AnObject <> nil) or (AnOperator <> nil));
  //TODO and attributes
end;

procedure TInstruction.SetOperator(AOperator: TSardOperator);
begin
  if (AnOperator <> nil) then
      RaiseError('Operator is already set');
  AnOperator := AOperator;
end;

procedure TInstruction.SetIdentifier(AIdentifier: string);
begin
  if (Identifier <> '') then
      RaiseError('Identifier is already set');
  Identifier := AIdentifier;
end;

function TInstruction.SetNumber(AIdentifier: string): TNumber_Node;
begin
  if (Identifier <> '') then
      RaiseError('Identifier is already set to ' +  Identifier);
  //TODO need to check object too
  if ((aIdentifier.IndexOf('.') >= 0) or ((aIdentifier.indexOf('E') >= 0))) then
    Result := TReal_Node.Create(StrToFloat(AIdentifier))
  else
    Result := TInteger_Node.Create(StrToInt(AIdentifier));

  InternalSetObject(Result);
end;

function TInstruction.SetText(Text: string): TText_Node;
begin
  {if (identifier <> '') then
      RaiseError("Identifier is already set");*}
  //TODO need review
  if (AnObject = nil) then
  begin
    Result := TText_Node.Create(text);
    InternalSetObject(Result);
  end
  else
  begin
    Result := (AnObject as TText_Node);
    if Result = nil then
       RaiseError('Object is already exist when setting string!');
    Result.Value := Result.Value + Text;
  end;
end;

function TInstruction.SetComment(AIdentifier: string): TComment_Node;
begin
  //We need to check if it the first expr in the statment
  if (Identifier <> '') then
      RaiseError('Identifier is already set');
  //TODO need to check object too
  Result := TComment_Node.Create;
  Result.Value := AIdentifier;
  InternalSetObject(Result);
end;

procedure TInstruction.SetObject(AObject: TNode);
begin
  if (Identifier <> '') then
    RaiseError('Identifier is already set');
  InternalSetObject(AObject);
end;

function TInstruction.SetInstance(AIdentifier: string): TInstance_Node;
begin
  if (Identifier = '') then
      RaiseError('Identifier is already set');
  Result := TInstance_Node.Create;
  Result.Name := AIdentifier;
  InternalSetObject(Result);
end;

function TInstruction.SetInstance: TInstance_Node;
begin
  if (Identifier = '') then
    RaiseError('Identifier is not set');
  Result := SetInstance(Identifier);
  Identifier := '';
end;

function TInstruction.SetEnclose: TEnclose_Node;
begin
  if (Identifier <> '') then
    RaiseError('Identifier is already set');
  Result := TEnclose_Node.Create;
  InternalSetObject(Result);
end;

function TInstruction.SetAssign: TAssign_Node;
begin
  //Do not check the Identifier if empty, becuase it is can be empty to assign to result of block
  Result := TAssign_Node.Create;
  Result.Name := Identifier;
  InternalSetObject(Result);
  Identifier := '';
end;

function TInstruction.SetDeclare: TDeclare_Node;
begin
  if (identifier = '') then
    RaiseError('Identifier is not set');
  Result := TDeclare_Node.Create;
  Result.Name := Identifier;
  InternalSetObject(Result);
  Identifier := '';
end;

end.
