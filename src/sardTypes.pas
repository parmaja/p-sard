unit sardTypes;
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
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils,
  sardClasses, sardLexers, sardObjects;

type

  { TFork_Node }

  TFork_Node = class(TNode)
  private
    FStatement: TStatement;
  protected
    procedure BeforeExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator); override;
    procedure AfterExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator); override;
    procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
  public
    procedure Created; override;
    destructor Destroy; override;
    property Statement: TStatement read FStatement;
  end;

  { TEnclose_Node }

  TEnclose_Node = class(TNode)
  private
    FStatements: TStatements;
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
  public
    procedure Created; override;
    destructor Destroy; override;
    property Statements: TStatements read FStatements;
  end;

  { TBlock_Node }

  TBlock_Node = class(TEnclose_Node)
  private
    FDeclareStatement: TStatement;
  protected
    procedure BeforeExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator); override;
    procedure AfterExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator); override;
  public
    procedure Created; override;
    destructor Destroy; override;
    function DeclareObject(AObject: TNode): TDeclare_Node;
    property DeclareStatement: TStatement read FDeclareStatement;
  end;

  { TConst_Node }

  TConst_Node = class(TNode)
  private
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
  public
  end;

//*-------------------------------*/
//*       Const Objects
//*-------------------------------*/

  TNone_Node = class(TConst_Node)
    //Do operator
    //Convert to 0 or ''
  end;

  { TComment_Node }

  TComment_Node = class(TNode)
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
  public
    Value: string;
  end;

  { TPreprocessor_Node }

  TPreprocessor_Node = class(TNode) //TODO
  protected
    procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
  public
  end;

  TNumber_Node = class abstract(TConst_Node)
  end;

  { TInteger_Node }

    TInteger_Node = class(TNumber_Node)
    public
      Value: Integer;
      constructor Create(AValue: Integer);
      procedure Assign(AFromObject: TNode); override;
      function DoOperate(AObject: TNode; AOperator: TSardOperator): Boolean; override;
      function ToText(out outValue: Text): Boolean;
      function ToNumber(out outValue: Number): Boolean;
      function ToBool(out outValue: Boolean): Boolean;
      function ToInteger(out outValue: Integer): Boolean;
    end;

    { TReal_Node }

    TReal_Node = class(TNumber_Node)
    public
      Value: Double;
      constructor Create(AValue: Number);
      procedure Assign(AFromObject: TNode); override;
      function DoOperate(AObject: TNode; AOperator: TSardOperator): Boolean; override;
      function ToText(out outValue: Text): Boolean;
      function ToNumber(out outValue: Number): Boolean;
      function ToBool(out outValue: Boolean): Boolean;
      function ToInteger(out outValue: Integer): Boolean;
    end;

    { TBool_Node }

    TBool_Node = class(TNumber_Node)
    public
      Value: Bool;
      constructor Create(AValue: Bool);
      procedure Assign(AFromObject: TNode); override;
      function DoOperate(AObject: TNode; AOperator: TSardOperator): Boolean; override;
      function ToText(out outValue: Text): Boolean;
      function ToNumber(out outValue: Number): Boolean;
      function ToBool(out outValue: Boolean): Boolean;
      function ToInteger(out outValue: Integer): Boolean;
    end;

    { TDate_Node }  //TODO

    {TDate_Node = class(TNumber_Node)
    public
      Value: TDateTime;
      constructor Create(AValue: TDateTime);
      procedure Assign(AFromObject: TNode); override;
      function DoOperate(AObject: TNode; AOperator: TSardOperator): Boolean; override;
      function ToText(out outValue: Text): Boolean;
      function ToNumber(out outValue: Number): Boolean;
      function ToBool(out outValue: Boolean): Boolean;
      function ToInteger(out outValue: Integer): Boolean;
    end;}

    { TText_Node }

    TText_Node = class(TConst_Node)
    public
      Value: string;
      constructor Create(AValue: string);
      procedure Assign(AFromObject: TNode); override;
      function DoOperate(AObject: TNode; AOperator: TSardOperator): Boolean; override;
      function ToText(out outValue: Text): Boolean;
      function ToNumber(out outValue: Number): Boolean;
      function ToBool(out outValue: Boolean): Boolean;
      function ToInteger(out outValue: Integer): Boolean;
    end;

    { TInstance_Node }

    TInstance_Node = class(TNode)
    private
      FArguments: TStatements;
    public
      procedure Created; override;
      destructor Destroy; override;
      procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
      property Arguments: TStatements read FArguments;
    end;

    { TAssign_Node }

    TAssign_Node = class(TNode)
    public
      procedure DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean); override;
    end;

implementation

{ TAssign_Node }

procedure TAssign_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
var
  v: TRunValue;
begin
  //* if not have a name, assign it to parent result
  Done := true;
  if (Name = '') then
    Env.Results.current.Result := Env.Results.Parent.Result
  else
  begin
    //Ok let is declare it locally
    v := Env.Stack.Current.Variables.Register(Name, [rkLocal]);
    if (v = nil) then
      RaiseError('Variable not found!');
    Env.Results.Current.Result := v;
  end;
end;

{ TInstance_Node }

procedure TInstance_Node.Created;
begin
  inherited;
  FArguments := TStatements.Create(Self);
end;

destructor TInstance_Node.Destroy;
begin
  FreeAndNil(FArguments);
  inherited;
end;

procedure TInstance_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
var
  d: TRunData;
  v: TRunValue;
begin
  d := Data.FindDeclare(name);
  if d <> nil then
    Done := d.Execute(env, AOperator, Arguments, nil)
  else
  begin
    v := Env.Stack.Current.Variables.Find(Name);
    if (v = nil) then
        RaiseError('Can not find a variable: ' + Name);
    if (v.value = nil) then
        RaiseError('Variable value is null: ' + v.Name);
    if (v.value = nil) then
        RaiseError('Variable object is null: ' + v.Name);
    Done := v.Value.Execute(Data, Env, AOperator);
  end;
end;

{ TText_Node }

constructor TText_Node.Create(AValue: string);
begin
  inherited Create;
  Value := AValue;
end;

procedure TText_Node.Assign(AFromObject: TNode);
begin
  inherited;
  Value := AFromObject.AsTExt;
end;

function TText_Node.DoOperate(AObject: TNode; AOperator: TSardOperator): Boolean;
begin
  Result :=inherited DoOperate(AObject, AOperator);
end;

function TText_Node.ToText(out outValue: Text): Boolean;
begin
  outValue := Value;
  Result := true;
end;

function TText_Node.ToNumber(out outValue: Number): Boolean;
begin
  outValue := StrToFloatDef(Value, 0);
  Result := true;
end;

function TText_Node.ToBool(out outValue: Boolean): Boolean;
begin
  outValue := StrToBoolDef(Value, False);
  Result := true;
end;

function TText_Node.ToInteger(out outValue: Integer): Boolean;
begin
  outValue := StrToIntDef(Value, 0);
  Result := true;
end;

{ TBool_Node }

constructor TBool_Node.Create(AValue: Bool);
begin
  inherited Create;
  Value := AValue;
end;

procedure TBool_Node.Assign(AFromObject: TNode);
begin
  inherited;
  Value := AFromObject.AsBool;
end;

function TBool_Node.DoOperate(AObject: TNode; AOperator: TSardOperator): Boolean;
begin
  Result := inherited DoOperate(AObject, AOperator);
  case AOperator.name of
      '+':
      begin
          Value := Value and AObject.AsBool;
          Result := true;
      end;
      '-':
      begin
          Value := Value and not AObject.AsBool; //xor //LOL
          Result := true;
      end;
      '*':
      begin
          Value := Value or AObject.AsBool;
          Result := True;
      end;
{      '/':
      begin
          Value := Value / AObject.AsNumber;
          Result := True;
      end;}
      else
          Result := False;
  end;
end;

function TBool_Node.ToText(out outValue: Text): Boolean;
begin
  outValue := BoolToStr(Value);
  Result := true;
end;

function TBool_Node.ToNumber(out outValue: Number): Boolean;
begin
  outValue := Ord(Value);
  Result := true;
end;

function TBool_Node.ToBool(out outValue: Boolean): Boolean;
begin
  outValue := Value;
  Result := true;
end;

function TBool_Node.ToInteger(out outValue: Integer): Boolean;
begin
  outValue := Ord(Value);
  Result := true;
end;

{ TReal_Node }

constructor TReal_Node.Create(AValue: Number);
begin
  inherited Create;
  Value := AValue;
end;

procedure TReal_Node.Assign(AFromObject: TNode);
begin
  inherited;
  Value := AFromObject.AsNumber;
end;

function TReal_Node.DoOperate(AObject: TNode; AOperator: TSardOperator): Boolean;
begin
  Result := inherited DoOperate(AObject, AOperator);
  case AOperator.name of
      '+':
      begin
          Value := Value + AObject.AsNumber;
          Result := true;
      end;
      '-':
      begin
          Value := Value - AObject.AsNumber;
          Result := true;
      end;
      '*':
      begin
          Value := Value * AObject.AsNumber;
          Result := True;
      end;
      '/':
      begin
          Value := Value / AObject.AsNumber;
          Result := True;
      end;
      else
          Result := False;
  end;
end;

function TReal_Node.ToText(out outValue: Text): Boolean;
begin
  outValue := FloatToStr(Value);
  Result := true;
end;

function TReal_Node.ToNumber(out outValue: Number): Boolean;
begin
  outValue := Value;
  Result := true;
end;

function TReal_Node.ToBool(out outValue: Boolean): Boolean;
begin
  outValue := Value <> 0;
  Result := true;
end;

function TReal_Node.ToInteger(out outValue: Integer): Boolean;
begin
  outValue := Round(Value);
  Result := true;
end;

{ TInteger_Node }

constructor TInteger_Node.Create(AValue: Integer);
begin
  inherited Create;
  Value := AValue;
end;

procedure TInteger_Node.Assign(AFromObject: TNode);
begin
  inherited;
  Value := AFromObject.AsInteger;
end;

function TInteger_Node.DoOperate(AObject: TNode; AOperator: TSardOperator): Boolean;
begin
  Result := inherited DoOperate(AObject, AOperator);
  case AOperator.name of
      '+':
      begin
          Value := Value + AObject.asInteger;
          Result := true;
      end;
      '-':
      begin
          Value := Value - AObject.AsInteger;
          Result := true;
      end;
      '*':
      begin
          Value := Value * AObject.AsInteger;
          Result := True;
      end;
      '/':
      begin
          Value := Value div AObject.AsInteger;
          Result := True;
      end;
      else
          Result := False;
  end;
end;

function TInteger_Node.ToText(out outValue: Text): Boolean;
begin
  outValue := IntToStr(Value);
  Result := True;
end;

function TInteger_Node.ToNumber(out outValue: Number): Boolean;
begin
  outValue := Value;
  Result := True;
end;

function TInteger_Node.ToBool(out outValue: Boolean): Boolean;
begin
  outValue := Value <> 0;
  Result := True;
end;

function TInteger_Node.ToInteger(out outValue: Integer): Boolean;
begin
  outValue := Value;
  Result := True;
end;

{ TPreprocessor_Node }

procedure TPreprocessor_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
begin
  //TODO execute external program and replace it with the result
  Done := True;
end;

{ TComment_Node }

procedure TComment_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
begin
  //Guess what!, we will not to execute the comment ;)
  Done := True;
end;

{ TConst_Node }

procedure TConst_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
begin
  if (Env.results.Current = nil) then
      RaiseError('There is no stack results!');
  if ((Env.Results.Current.Result.Value = nil) and (AOperator = nil)) then
  begin
      Env.Results.Current.Result.Value := Clone();
      Done := true;
  end
  else
  begin
      if (Env.Results.current.Result.Value = nil) then
          Env.Results.Current.Result.Value := Clone(False);
      Done := Env.Results.Current.Result.Value.Operate(self, AOperator);
  end;
end;

{ TBlock_Node }

procedure TBlock_Node.BeforeExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator);
begin
  Env.Stack.Push;
  inherited;
end;

procedure TBlock_Node.AfterExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator);
begin
  inherited;
  Env.Stack.Pop;
end;

procedure TBlock_Node.Created;
begin
  inherited;
  FDeclareStatement := TStatement.Create(Parent);
end;

destructor TBlock_Node.Destroy;
begin
  FreeAndNil(FDeclareStatement);
  inherited Destroy;
end;

function TBlock_Node.DeclareObject(AObject: TNode): TDeclare_Node;
begin
  if (DeclareStatement = nil) then
      FDeclareStatement :=  Statements.add;
  Result := TDeclare_Node.Create;
  //TODO is parent should be nil
  Result.Name := AObject.Name;
  Result.ExecuteObject := AObject;
  DeclareStatement.Add(nil, Result);
end;

{ TEnclose_Node }

procedure TEnclose_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
var
  t: TRunResult;
begin
  {if (env.stack.current.data.object !is this)
      error("Can not execute block directly, data.object must set to this encloser");}
  Env.Results.Push; //<--here we can push a variable result or create temp result to drop it

  Statements.Execute(Data, Env);
  t := Env.Results.Pull;
  //I dont know what if there is an object there what we do???
(*
  * := 5 + { := 10 + 10 }
  * it return 25
  * here 20.execute with +
*)
  if (t.Result.Value <> nil) then
      t.Result.Value.Execute(Data, Env, AOperator);
  Done := True;
end;

procedure TEnclose_Node.Created;
begin
  inherited;
  FStatements := TStatements.Create(Parent);
end;

destructor TEnclose_Node.Destroy;
begin
  FreeAndNil(FStatements);
  inherited;
end;

{ TFork_Node }

procedure TFork_Node.BeforeExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator);
begin
  inherited;
  Env.Results.Push;
end;

procedure TFork_Node.AfterExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator);
var
  t: TRunResult;
begin
  inherited;
  t := Env.Results.Pull;
  if (t.Result.Value <> nil) then
    t.Result.Value.Execute(Data, Env, AOperator);
end;

procedure TFork_Node.DoExecute(Data: TRunData; Env: TRunEnv; AOperator: TSardOperator; var Done: Boolean);
begin
  statement.execute(data, env);
  Done := true;
end;

procedure TFork_Node.Created;
begin
  inherited Created;
  FStatement := TStatement.Create(Parent);
end;

destructor TFork_Node.Destroy;
begin
  FreeAndNil(FStatement);
  inherited;
end;

end.
