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
  sardClasses, sardObjects, sardLexers, sardScanners, sardParsers;

const
  sWhitespace = sEOL + [' ', #8];
  sNumberOpenChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
  sNumberChars = sNumberOpenChars + ['.', 'x', 'h', 'a', 'b', 'c', 'd', 'e', 'f'];
  sSymbolChars = ['"', '''', '\']; //deprecated;
  sIdentifierSeparator = '.';

type
  { TCodeScanner }

  TCodeScanner = class(TScanner)
  protected
    Block: TBlock_Node;
    Parser: TCodeParser;
    procedure DoStart; override;
    procedure DoStop; override;
  public
    constructor Create(ABlock: TBlock_Node);
  end;

  { TCodeLexer }

  TCodeLexer = class(TLexer)
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

  { TCodeParser }

  TCodeParser = class(TParser)
  protected
    LastControl: TSardControlID;
    Lexer: TLexer;
    function IsKeyword(AIdentifier: string): Boolean; override;
    procedure SetToken(Token: TSardToken); override;
    procedure SetOperator(AOperator: TSardOperator); override;
    procedure SetControl(AControl: TSardControl); override;
    procedure SetWhiteSpaces(Whitespaces: string); override;
    procedure AfterPush; override;
    procedure BeforePop; override;
    procedure DoQueue;

  public
    constructor Create(ALexer: TLexer; AStatements: TStatements);
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
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

  TSardScript = class(TSardObject)
  public
    Main: TBlock_Node;
    Scanner: TScanner;
    Result: string;
    destructor Destroy; override;
    procedure Compile(Text: string); overload;
    procedure Compile(Lines: TStringList); overload;
    procedure Run;
  end;

implementation

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
  Current.AddOperator(o);
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

procedure TCodeScanner.DoStart;
begin
  inherited DoStart;
  Parser := TCodeParser.Create(Lexer, Block.Statements);

  Lexer.Parser := Parser;
  Lexer.Start;
end;

procedure TCodeScanner.DoStop;
begin
  Lexer.Stop;
  Lexer.Parser := nil;
  FreeAndNil(Parser);
  inherited;
end;

constructor TCodeScanner.Create(ABlock: TBlock_Node);
begin
  inherited Create;
  Block := ABlock;
  Add(TCodeLexer.Create);
end;

{ TSardScript }

destructor TSardScript.Destroy;
begin
  FreeAndNil(Main);
  FreeAndNil(Scanner);
  inherited Destroy;
end;

procedure TSardScript.Compile(Lines: TStringList);
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
  Print_Object.name := 'print';
  with Main.DeclareObject(Print_Object) do
    Defines.Parameters.Add('s', 'string');

  // Compile

  Scanner := TCodeScanner.Create(Main);

//  debug(log_compile) writeln("-------- Scanning --------");
  Scanner.Scan(Lines);
end;

procedure TSardScript.Run;
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

procedure TSardScript.Compile(Text: string);
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

{ TPrint_Object_Node }

procedure TPrint_Object_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
var
  v: TRunValue;
begin
  v := Env.Stack.Current.Variables.Find('s');
  if (v <> nil) and (v.Value <> nil) then
  begin
    WriteLn(V.Value.AsText);
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

end.
