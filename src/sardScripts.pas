unit sardScripts;
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

interface

uses
  Classes, SysUtils,
  sardClasses, sardObjects, sardTypes, sardLexers, sardScanners, sardParsers;

type
  { TCodeScanner }

  TCodeScanner = class(TScanner)
  protected
    Block: TBlock_Node;
    procedure DoStart; override;
    procedure DoStop; override;
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

{ TCodeScanner }

procedure TCodeScanner.DoStart;
var
  Parser: TCodeParser;
begin
  inherited DoStart;
  Parser := TCodeParser.Create(Lexer, Block.Statements);

  Lexer.Parser := Parser;
  Lexer.Start();

  FreeAndNil(Parser);
end;

procedure TCodeScanner.DoStop;
begin
  Lexer.stop;
  Lexer.Parser := nil;
  inherited;
end;

constructor TCodeScanner.Create(ABlock: TBlock_Node);
begin
  inherited Create;
  Block:= ABlock;
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
  Print_Declare: TDeclare_Node;
begin
  //writeln("-------------------------------");

  FreeAndNil(Main);

  Main := TBlock_Node.Create; //destory the old compile and create new
  Main.Name := 'main';

  Version_Const := TVersion_Const_Node.Create;
  Version_Const.Name := 'Version';
  Main.DeclareObject(Version_Const);

  PI_const := TPI_Const_Node.Create();
  PI_Const.name := 'PI';
  Main.DeclareObject(PI_const);

  Print_Object := TPrint_object_Node.Create();
  Print_Object.name := 'print';
  Print_Declare := Main.DeclareObject(Print_Object);
  Print_Declare.Defines.Parameters.Add('s', 'string');

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
    //Env.Root.Object := main;
    Main.Execute(Env.Root, Env, nil);

    if (Env.Results.Current <> nil) and (Env.Results.Current.Result.Value <> nil) then
    begin
      Result := Env.Results.Current.Result.Value.AsText;
    end;
    Env.Results.Pop();
  finally
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
  if (v <> nil) then
  begin
      //if (v.value !is null) //TODO it is bad, we should not have it null
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
