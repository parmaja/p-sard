unit sardScripts;
{**
*  This file is part of the "SARD"
*
* @license   The MIT License (MIT)
*            Included in this distribution
* @author    Zaher Dirkey
*}

{$IFDEF FPC}
{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils,
  sardClasses, sardObjects, sardParsers, sardStandards;

type
  { TCodeLexer }

  TCodeLexer = class(TLexer)
  protected
    const
      sNumberOpenChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
      sNumberChars = sNumberOpenChars + ['.', 'x', 'h', 'a', 'b', 'c', 'd', 'e', 'f'];
  public
    constructor Create; override;
    function IsNumber(const vChar: Char; vOpen: Boolean =true): Boolean; override;
    function IsIdentifier(const vChar: Char; vOpen: Boolean =true): Boolean;
  end;

  TCodeParser = class;

  { TInstruction }

  TInstruction = record
  public
    procedure InternalSetObject(aObject: TNode);
  public
    Identifier: string;
    AnObject: TNode;
    //Return true if Identifier is not empty and object is nil
    function CheckIdentifier(raiseIt: Boolean = false): Boolean;
    //Return true if Object is not nil and Identifier is empty
    function CheckObject(raiseIt: Boolean = false): Boolean;
    function IsEmpty: Boolean;
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
    procedure Prepare; virtual;
    procedure Post; virtual;
    procedure Next; virtual;
    procedure DoToken(Token: TSardToken); override;
    procedure DoControl(AControl: TSardControl); override;

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
    procedure DoControl(AControl: TSardControl); override;
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
    procedure DoControl(AControl: TSardControl); override;
    procedure Reset; override;
    function IsInitial: Boolean; override;
  end;

  { TCodeParser }

  TCodeParser = class(TParser)
  protected
    LastControl: TSardControlID;
    Lexer: TLexer;
  public
    ControlEnd: TSardControl;
    constructor Create(ALexer: TLexer; AStatements: TStatements); reintroduce;
    destructor Destroy; override;

    function IsKeyword(const AIdentifier: string): Boolean; override;
    procedure SetToken(Token: TSardToken); override;
    procedure SetControl(AControl: TSardControl); override;
    procedure AfterPush; override;
    procedure BeforePop; override;
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
    procedure DoExecute(Data: TRunData; Env: TRunEnv; var Done: Boolean); override;
  end;

  { TPI_Const_Node }

  TPI_Const_Node = class(TNode)
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; var Done: Boolean); override;
  end;

  { TTime_Const_Node }

  TTime_Const_Node = class(TNode)
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; var Done: Boolean); override;
  end;

  { TPrint_Object_Node }

  TPrint_Object_Node = class(TNode)
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; var Done: Boolean); override;
  end;

  { TCodeScript }

  TCodeScript = class(TSardObject)
  protected
    procedure RegisterStandard;
  public
    Main: TMain_Node;
    Scanner: TScanner;
    RegisterInternals: Boolean;
    destructor Destroy; override;
    procedure Init;
    procedure Compile(Lines: TStringList); overload;
    procedure Compile(Text: string); overload;
    procedure Run;
    procedure ExportToFile(FileName: string);
    procedure ExportToConsole;
  end;

implementation

{ TCodeCollector }

procedure TCodeCollector.Reset;
begin
  //FreeAndNil(instruction);
  FInstruction := Default(TInstruction);
  //instruction= TInstruction.Create;
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

procedure TCodeCollector.DoToken(Token: TSardToken);
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

procedure TCodeCollector.DoControl(AControl: TSardControl);
begin
end;

{procedure TCodeCollector.SetOperator(AOperator: TSardOperator);
begin
  Post;
  Instruction.SetOperator(AOperator);
end;}

{ TCodeLexer }

constructor TCodeLexer.Create;
begin
  inherited;

  with (Self) do
  begin
      Add(TWhitespace_Tokenizer.Create);
      Add(TML_Comment_Tokenizer.Create);
      Add(TSardComment_Tokenizer.Create);
      Add(TSL_Comment_Tokenizer.Create);
      Add(TNumber_Tokenizer.Create);
      Add(TML_SQString_Tokenizer.Create);
      Add(TML_DQString_Tokenizer.Create);
      Add(TOut_Escape_Tokenizer.Create);

      Add(TControl_Tokenizer.Create(':=', ctlAssign)); // Logner is first please
      Add(TControl_Tokenizer.Create(':', ctlDeclare));
      Add(TControl_Tokenizer.Create('(', ctlOpenParams));
      Add(TControl_Tokenizer.Create('[', ctlOpenArray));
      Add(TControl_Tokenizer.Create('{', ctlOpenBlock));
      Add(TControl_Tokenizer.Create(')', ctlCloseParams));
      Add(TControl_Tokenizer.Create(']', ctlCloseArray));
      Add(TControl_Tokenizer.Create('}', ctlCloseBlock));
      Add(TControl_Tokenizer.Create(';', ctlEnd));
      Add(TControl_Tokenizer.Create(',', ctlNext));

      Add(TIdentifier_Tokenizer.Create);//Sould be last one
  end;
end;

function TCodeLexer.IsNumber(const vChar: Char; vOpen: Boolean): Boolean;
begin
  if (vOpen) then
    Result := CharInSet(vChar, sNumberOpenChars)
  else
    Result := CharInSet(vChar, sNumberChars);
end;

function TCodeLexer.IsIdentifier(const vChar: Char; vOpen: Boolean): Boolean;
begin
  Result := inherited isIdentifier(vChar, vOpen); //we do not need to override it, but it is nice to see it here
end;

{ TCodeParser }

function TCodeParser.IsKeyword(const AIdentifier: string): Boolean;
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
      SetControl(ControlEnd);
    end;

    inherited;
    LastControl := ctlToken;
  end;
end;

{procedure TCodeParser.SetOperator(AOperator: TSardOperator);
var
  o: TSardOperator;
begin
  inherited;
  o := AOperator;
  if (o = nil) then
    RaiseError('SetOperator not Operator');
  (Current as TCodeCollector).SetOperator(o);
  DoQueue();
  FActions := [];
  LastControl := ctlOperator;
end;}

procedure TCodeParser.SetControl(AControl: TSardControl);
begin
  if (LastControl = ctlCloseBlock) then //see setToken
  begin
      LastControl := ctlNone;//prevent loop
      SetControl(ControlEnd); //TODO check if we need it
  end;

  inherited;

  LastControl := aControl.Code;
end;

procedure TCodeParser.AfterPush;
begin
  inherited;
end;

procedure TCodeParser.BeforePop;
begin
  inherited;
end;

constructor TCodeParser.Create(ALexer: TLexer; AStatements: TStatements);
begin
  inherited Create(False); //TODO Check if own
  ControlEnd := TSardControl.Create('', ctlEnd, '');
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

{ TCodeScanner }

function TCodeScanner.CreateParser: TParser;
begin
  Result := TCodeParser.Create(Current, Block.Statements);
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
  inherited;
end;

procedure TCodeScript.RegisterStandard;
var
  Version_Const: TVersion_Const_Node;
  PI_Const: TPI_Const_Node;
  Print_Object: TPrint_object_Node;
begin
  Version_Const := TVersion_Const_Node.CreateInternal;
  Version_Const.Name := 'Version';
  Main.DeclareObject(Version_Const);

  PI_const := TPI_Const_Node.CreateInternal;
  PI_Const.name := 'PI';
  Main.DeclareObject(PI_Const);

  Print_Object := TPrint_object_Node.CreateInternal;
  Print_Object.Name := 'print';
  with Main.DeclareObject(Print_Object) do
    Defines.Parameters.Add('s', 'string');
end;

procedure TCodeScript.Compile(Lines: TStringList);
begin
  //writeln("-------------------------------");
  Init;
  // Compile
  Scanner := TCodeScanner.Create(Main);
  Scanner.Scan(Lines);
end;

procedure TCodeScript.Compile(Text: string);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Text;
    Compile(Lines);
  finally
    FreeAndNil(Lines);
  end;
end;

procedure TCodeScript.Run;
var
  Env: TRunEnv;
begin
  Env := TRunEnv.Create;
  try
    Main.Execute(Env.Root, Env, nil);
  finally
    FreeAndNil(Env);
  end;
end;

procedure TCodeScript.ExportToFile(FileName: string);
var
  Writer: TStringSourceWriter;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  Writer := TStringSourceWriter.Create(Strings);
  try
    Main.ExportWrite(Writer, False, 0);
    Writer.Flush;
    Strings.SaveToFile(FileName);
  finally
    FreeAndNil(Writer);
    FreeAndNil(Strings);
  end
end;

procedure TCodeScript.Init;
begin
  FreeAndNil(Main);
  Main := TMain_Node.Create; //destory the old compile and create new
//  Main.Name := 'main';
  if RegisterInternals then
    RegisterStandard;
end;

procedure TCodeScript.ExportToConsole;
var
  Writer: TStringSourceWriter;
  Strings: TStringList;
  s: string;
begin
  Strings := TStringList.Create;
  Writer := TStringSourceWriter.Create(Strings);
  try
    Main.ExportWrite(Writer, False, 0);
    Writer.Flush;
    for s in Strings do
      WriteLn(s);
  finally
    FreeAndNil(Writer);
    FreeAndNil(Strings);
  end
end;

{ TPrint_Object_Node }

procedure TPrint_Object_Node.DoExecute(Data: TRunData; Env: TRunEnv; var Done: Boolean);
var
  v: TRunVariable;
begin
  v := Env.Stack.Current.Variables.Find('s');
  if (v <> nil) and (v.Value <> nil) then
  begin
    WriteLn(V.Value.AsText);
    Done := true;
  end;
end;

{ TTime_Const_Node }

procedure TTime_Const_Node.DoExecute(Data: TRunData; Env: TRunEnv; var Done: Boolean);
begin
  Env.Results.Current.Value := TReal_Node.Create(Now);
  Done := True;
end;

{ TPI_Const_Node }

procedure TPI_Const_Node.DoExecute(Data: TRunData; Env: TRunEnv; var Done: Boolean);
begin
  Env.Results.Current.Value := TReal_Node.Create(Pi);
  Done := True;
end;

{ TVersion_Const_Node }

procedure TVersion_Const_Node.DoExecute(Data: TRunData; Env: TRunEnv; var Done: Boolean);
begin
  Env.Results.Current.Value := TText_Node.Create(sSardVersion);
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

procedure TCollectorDefine.DoControl(AControl: TSardControl);
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

procedure TCollectorDeclare.DoControl(AControl: TSardControl);
begin
  case (AControl.code) of
    ctlEnd, ctlNext:
    begin
      Post;
      Parser.SetAction([paPop, paPass]);
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
  Statement.Add(Instruction.AnObject);
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

procedure TInstruction.InternalSetObject(aObject: TNode);
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

function TInstruction.IsEmpty: Boolean;
begin
  Result := not((Identifier <> '') or (AnObject <> nil));
  //TODO and attributes
end;

{procedure TInstruction.SetOperator(AOperator: TSardOperator);
begin
  if (AnOperator <> nil) then
      RaiseError('Operator is already set');
  AnOperator := AOperator;
end;}

procedure TInstruction.SetIdentifier(AIdentifier: string);
begin
  if (Identifier <> '') then
      RaiseError('Identifier is already set: ' + Identifier);
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
  Result := TAssign_Node.Create(Identifier);
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
