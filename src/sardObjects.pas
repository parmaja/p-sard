unit sardObjects;
{**
 *  This file is part of the "SARD"
 *
 * @license   Apache License Version 2.0
 *            included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}{$M+}
{$INTERFACES CORBA}

(*

  o1: {
    o2: {
    }
    fn1;
    fn2;
  }

 o1;
 fn1;
 fn2;
 o1.o2;

 o3 := 10+5;
*)

{
TODO:
create variable manually in the example to test return value in formual
10+x
soVariable find it is value in the stack and use it in formual

soVariable create a runtime variable in the stack
x := 10;
}

{
  Prefix guid
  srd global classes inherited from sard
  run Runtime classes
  so Sard objects, that created when compile the source
  op Operators objects
}
interface

uses
  Classes, SysUtils, sardClasses;

type

  TsrdObjectType = (otUnkown, otInteger, otFloat, otBoolean, otString, otComment, otBlock, otObject, otClass, otVariable);
  TsrdCompare = (cmpLess, cmpEqual, cmpGreater);

  TsrdDebug = class(TsardObject)
  public
    Line: Integer;
    Column: Integer;
    FileName: string;
    BreakPoint: Boolean; //not sure do not laugh
  end;

  TsoObject = class;
  TsoObjectClass = class of TsoObject;

  TopOperator = class;
  TrunResult = class;
  TrunStack = class;
  TrunVariables = class;

  TsoBlock = class;

  { TsrdStatementItem }

  TsrdStatementItem = class(TsardObject)
  public
    AnOperator: TopOperator;
    AnObject: TsoObject;
    function Execute(vStack: TrunStack): Boolean;
  end;

  { TsrdObjectList }

  TsrdObjectList = class(TsardObjectList)
  private
    FParent: TsoBlock;
  public
    constructor Create(AParent: TsoBlock);
    property Parent: TsoBlock read FParent;
  end;

  { TsrdStatement }

  TsrdStatement = class(TsrdObjectList)
  private
    FDebug: TsrdDebug;
    function GetItem(Index: Integer): TsrdStatementItem;
    procedure SetDebug(AValue: TsrdDebug);
  protected
  public
    function Add(AObject: TsrdStatementItem): Integer;
    function Add(AOperator:TopOperator; AObject: TsoObject): TsrdStatementItem;
    function Execute(vStack: TrunStack): Boolean;
    property Items[Index: Integer]: TsrdStatementItem read GetItem; default;
    property Debug: TsrdDebug read FDebug write SetDebug; //<-- Nil until we compile it with Debug Info
  end;

  { TsrdBlock }

  TsrdBlock = class(TsrdObjectList)
  private
    function GetStatement: TsrdStatement;
    function GetItem(Index: Integer): TsrdStatement;
  protected
  public
    function Add(AStatement: TsrdStatement): Integer;
    function Add: TsrdStatement;
    procedure Check; //Check if empty then create first statement
    property Items[Index: Integer]: TsrdStatement read GetItem; default;
    property Statement: TsrdStatement read GetStatement;
    function Execute(vStack: TrunStack): Boolean;
  end;

  { TsrdBlockStack }

  TsrdBlockStack = class(TsardStack)
  private
    function GetCurrent: TsrdBlock;
  public
    procedure Push(vItem: TsrdBlock);
    property Current: TsrdBlock read GetCurrent;
  end;

  IsrdObject = interface['{9FD9AEE0-507E-4EEA-88A8-AE686E6A1D98}']
  end;

  IsrdOperate = interface['{4B036431-57FA-4E6D-925C-51BC1B67331A}']
    function Add(var vResult: TrunResult): Boolean;
    function Sub(var vResult: TrunResult): Boolean;
    function Mulpiply(var vResult: TrunResult): Boolean;
    function Divide(var vResult: TrunResult): Boolean;
  end;

  IsrdCompare = interface['{4B036431-57FA-4E6D-925C-51BC1B67331A}']
    function Compare(var vResult: TrunResult): TsrdCompare;
  end;

  IsrdBlock = interface['{CB4C0FA1-E233-431E-8CC2-3755F62D93F2}']
    function Execute(vStack: TrunStack; AOperator: TopOperator): Boolean;
  end;

  { TsoObject }

  TsoObject = class abstract(TsardObject, IsrdObject)
  private
  protected
    FObjectType: TsrdObjectType;
    function GetAsString: String;
    function GetAsFloat: Float;
    function GetAsInteger: int;
    function GetAsBoolean: Boolean;

    function Operate(AObject: TsoObject; AOperator: TopOperator): Boolean; virtual;
    procedure BeforeExecute(vStack: TrunStack; AOperator: TopOperator); virtual;
    procedure AfterExecute(vStack: TrunStack; AOperator: TopOperator); virtual;
    function DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean; virtual; abstract;
  public
    constructor Create; virtual;
    function This: TsoObject; //return the same object, stupid but save some code :P
    function Execute(vStack: TrunStack; AOperator: TopOperator): Boolean;
    procedure Assign(FromObject: TsoObject); virtual;
    function Clone(WithValue: Boolean = True): TsoObject; virtual;
    property ObjectType: TsrdObjectType read FObjectType;

    function ToBoolean(out outValue: Boolean): Boolean; virtual;
    function ToString(out outValue: string): Boolean; virtual; reintroduce;
    function ToFloat(out outValue: Float): Boolean; virtual;
    function ToInteger(out outValue: int): Boolean; virtual;

    property AsInteger: Int read GetAsInteger;
    property AsFloat: Float read GetAsFloat;
    property AsString: string read GetAsString;
    property AsBoolean: Boolean read GetAsBoolean;
  end;

  { TsoObjects }

  TsrdObjects = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TsoObject;
  public
    property Items[Index: Integer]: TsoObject read GetItem; default;
  end;

  {-------- Objects --------}

  { TsoConstObject }

  TsoConstObject = class abstract(TsoObject)
  protected
    function DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean; override; final;
  public
  end;

  { TsoParentedObject }

  TsoParentedObject = class(TsoObject)
  private
    FParent: TsoBlock;
    procedure SetParent(AValue: TsoBlock);
  protected
  public
    property Parent: TsoBlock read FParent write SetParent;
  end;

  { TsoBlock }

  TsoBlock = class abstract(TsoParentedObject, IsrdBlock)
  protected
    FItems: TsrdBlock;
    procedure Created; override;
    function DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Items: TsrdBlock read FItems;
  end;

  { TsoSection }
  (* Used by { } *)

  TsoSection = class(TsoBlock) //Result of was droped until using := assign in the first of statment
  protected
    procedure BeforeExecute(vStack: TrunStack; AOperator: TopOperator); override;
    procedure AfterExecute(vStack: TrunStack; AOperator: TopOperator); override;
  public
  end;

  { TsoStatement }

  TsoStatement = class(TsoParentedObject)
  private
    FStatement: TsrdStatement;
  protected
    procedure BeforeExecute(vStack: TrunStack; AOperator: TopOperator); override;
    procedure AfterExecute(vStack: TrunStack; AOperator: TopOperator); override;
    function DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Statement: TsrdStatement read FStatement;
  end;

  { TsoNamedObject }

  TsoNamedObject = class(TsoParentedObject)
  private
    FID: Integer;
    FName: string;
    procedure SetID(AValue: Integer);
    procedure SetName(AValue: string);
  public
    property Name: string read FName write SetName;
    property ID: Integer read FID write SetID;
  end;

  { TsoNamedBlock }

  TsoNamedBlock = class(TsoBlock)
  private
    FID: Integer;
    FName: string;
    procedure SetID(AValue: Integer);
    procedure SetName(AValue: string);
  public
    property Name: string read FName write SetName;
    property ID: Integer read FID write SetID;
  end;

  {*  Variables objects *}

  { TsoInstance }

  { it is a variable value like x in this "10 + x + 5" }

  TsoInstance = class(TsoNamedObject)
  private
  protected
    function DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean; override;
  public
    procedure Created; override;
    procedure FindMe;
  end;

  { TsoAssign }

  { It is assign a variable value, x:=10 + y}

  TsoAssign = class(TsoNamedObject)
  private
  protected
    procedure Created; override;
    function DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean; override;
  public
  end;

  { TsoDeclare }

  TsoDeclare = class(TsoNamedBlock)
  private
  protected
    procedure Created; override;
    function DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean; override;
  public
  end;

  { TsoMain }

  TsoMain = class(TsoSection)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{-------- Const Objects --------}

  { TsrdNone }

  TsoNone = class(TsoConstObject) //None it is not Null, it is an initial value we sart it
  public
    //Do operator
    //Convert to 0 or ''
  end;

  { TsrdNumber }

  TsoNumber = class(TsoConstObject) //abstract
  public
    //Assign
    //Do operator
  end;

  { TsoInteger }

  TsoInteger = class(TsoNumber)
  protected
    procedure Created; override;
  public
    Value: int;
    constructor Create(AValue: int); overload;
    procedure Assign(FromObject: TsoObject); override;
    function Operate(AObject: TsoObject; AOperator: TopOperator): Boolean; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsrdFloat }

  { TsoFloat }

  TsoFloat = class(TsoNumber)
  public
    Value: Float;
    procedure Created; override;
    procedure Assign(FromObject: TsoObject); override;
    function Operate(AObject: TsoObject; AOperator: TopOperator): Boolean; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsrdString }

  { TsoString }

  TsoString = class(TsoConstObject)
  public
    Value: string;
    procedure Created; override;
    procedure Assign(FromObject: TsoObject); override;
    function Operate(AObject: TsoObject; AOperator: TopOperator): Boolean; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsrdBoolean }

  TsoBoolean = class(TsoNumber)
  public
    Value: Boolean;
    procedure Created; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsoComment }

  TsoComment = class(TsoObject)
  protected
    function DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean; override;
  public
    Value: string;
    procedure Created; override;
  end;

{-------- Controls  --------}

  { TctlControl }

  TctlControl = class(TsardObject)
  protected
  public
    Name: string;
    Code: TsardControl;
    Level: Integer;
    Description: string;
    constructor Create; virtual;
  end;

  TctlControlClass = class of TctlControl;

  { TctlControls }

  TctlControls = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TctlControl;
  protected
    function Check(AControl: TctlControl): Boolean; virtual;
  public
    function Find(const vName: string): TctlControl;
    function Add(AControl: TctlControl): Boolean;
    function Add(AControlClass: TctlControlClass): Boolean;
    function Add(AName: string; ACode: TsardControl): TctlControl;
    function Scan(const vText: string; vIndex: Integer): TctlControl;
    function IsOpenBy(const C: Char): Boolean;
    property Items[Index: Integer]: TctlControl read GetItem; default;
  end;

{-------- Operators --------}

  { TopOperator }

  TopOperator = class(TsardObject)
  protected
    function DoExecute(vStack: TrunStack; vObject: TsoObject): Boolean; virtual;
  public
    Name: string;
    Title: string;
    Level: Integer;
    Description: string;
    function Execute(vStack: TrunStack; vObject: TsoObject): Boolean;
    constructor Create; virtual;
  end;

  TopOperatorClass = class of TopOperator;

  { TopOperators }

  TopOperators = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TopOperator;
  protected
    function Check(AOperator: TopOperator): Boolean; virtual;
  public
    function Find(const vName: string): TopOperator;
    function FindByTitle(const vTitle: string): TopOperator;
    function Add(AOperator: TopOperator): Boolean;
    function Add(AOperatorClass: TopOperatorClass): Boolean;
    function IsOpenBy(const C: Char): Boolean;
    function Scan(const vText: string; vIndex: Integer): TopOperator;
    property Items[Index: Integer]: TopOperator read GetItem; default;
  end;

  { TopPlus }

  TopPlus = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopMinus }

  TopMinus = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopMultiply }

  TopMultiply = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopDivide }

  TopDivide = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopPower }

  TopPower = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopLesser }

  TopLesser = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopGreater }

  TopGreater = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopEqual }

  TopEqual = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopNotEqual }

  TopNot = class(TopOperator)
  public
    constructor Create; override;
  end;

  TopNotEqual = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopAnd }

  TopAnd = class(TopOperator)
  public
    constructor Create; override;
  end;

  { TopOr }

  TopOr = class(TopOperator)
  public
    constructor Create; override;
  end;

{-------- Run Time Engine --------}

  { TrunData }

  TrunData = class(TsardObject)
  private
    FCount: Integer;
  public
    //Objects:
    function RegisterID(vName: string): Integer;
    property Count: Integer read FCount;
  end;

  { TrunVariable }

  TrunVariable = class(TsardObject)
  private
    FName: string;
    procedure SetName(AValue: string);
  public
    Value: TrunResult;
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write SetName;
  end;

  { TrunVariables }

  TrunVariables = class(TsardObjectList)
  private
    function GetItem(Index: Integer): TrunVariable;
  public
    property Items[Index: Integer]: TrunVariable read GetItem; default;
    function Find(vName: string): TrunVariable;
    function Register(vName: string): TrunVariable;
    function SetValue(vName: string; vValue: TsoObject): TrunVariable;
  end;

  { TrunResult }

  TrunResult = class(TsardObject)
  private
    FAnObject: TsoObject;
    procedure SetAnObject(AValue: TsoObject);
  public
    destructor Destroy; override;
    function HasValue: Boolean;
    procedure Assign(AResult: TrunResult); virtual;
    function Extract: TsoObject;
    property AnObject: TsoObject read FAnObject write SetAnObject;
  end;

  { TrunScope }

  TrunScopeItem = class(TsardObject)
  private
    FVariables: TrunVariables;
  public
    constructor Create;
    destructor Destroy; override;
    property Variables: TrunVariables read FVariables;
  end;

  { TrunScopes }

  TrunScope = class(TsardStack)
  private
    function GetCurrent: TrunScopeItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(vObject: TrunScopeItem);
    function Push: TrunScopeItem; overload;
    function Pull: TrunScopeItem;
    property Current: TrunScopeItem read GetCurrent;
  end;

  { TrunStackItem }

  TrunStackItem = class(TsardObject)
  private
    FResult: TrunResult;
    FReference: TrunResult; //nil but if it exist we use it to assign it after statment executed
    procedure SetReference(AValue: TrunResult);
  public
    constructor Create;
    destructor Destroy; override;
    property Result: TrunResult read FResult;
    property Reference: TrunResult read FReference write SetReference;
  end;

  { TrunStack }

  TrunStack = class(TsardStack)
  private
    FData: TrunData;
    FScope: TrunScope;
    function GetCurrent: TrunStackItem;
    function GetParent: TrunStackItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(vObject: TrunStackItem);
    function Push: TrunStackItem; overload;
    function Pull: TrunStackItem;
    property Current: TrunStackItem read GetCurrent;
    property Parent: TrunStackItem read GetParent;
    property Data: TrunData read FData;
    property Scope: TrunScope read FScope;
  end;

  { TsrdEngine }

  TsrdEngine = class(TsardPrimeEngine)
  private
  protected
    procedure Created; override;
  public
    constructor Create;
  end;

function sardEngine: TsrdEngine;

implementation

var
  FsardEngine: TsrdEngine = nil;

function sardEngine: TsrdEngine;
begin
  if FsardEngine = nil then
    FsardEngine := TsrdEngine.Create;
  Result := FsardEngine;
end;

{ TsoStatement }

constructor TsoStatement.Create;
begin
  inherited Create;
  FStatement := TsrdStatement.Create(Parent);
end;

destructor TsoStatement.Destroy;
begin
  FreeAndNil(FStatement);
  inherited Destroy;
end;

procedure TsoStatement.BeforeExecute(vStack: TrunStack; AOperator: TopOperator);
begin
  vStack.Push;
end;

procedure TsoStatement.AfterExecute(vStack: TrunStack; AOperator: TopOperator);
var
  T: TrunStackItem;
begin
  inherited;
  T := vStack.Pull;
  if T.Result.AnObject <> nil then
    T.Result.AnObject.Execute(vStack, AOperator);
  FreeAndNil(T);
end;

function TsoStatement.DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean;
begin
  //Result := inherited Execute(vStack, AOperator);

  Result := FStatement.Execute(vStack);
end;

{ TsoComment }

function TsoComment.DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean;
begin
  Result := inherited Execute(vStack, AOperator);//Nothing to do compiled comments not executed
end;

procedure TsoComment.Created;
begin
  inherited Created;
  FObjectType := otComment;
end;

{ TrunScope }

function TrunScope.GetCurrent: TrunScopeItem;
begin
  Result := (inherited GetCurrent) as TrunScopeItem;
end;

constructor TrunScope.Create;
begin

end;

destructor TrunScope.Destroy;
begin
  inherited Destroy;
end;

procedure TrunScope.Push(vObject: TrunScopeItem);
begin
  inherited Push(vObject);
end;

function TrunScope.Push: TrunScopeItem;
begin
  Result := TrunScopeItem.Create;
  Push(Result);
end;

function TrunScope.Pull: TrunScopeItem;
begin
  Result := (inherited Pull) as TrunScopeItem;
end;

{ TctlControl }

constructor TctlControl.Create;
begin
  inherited Create;
end;

{ TctlControls }

function TctlControls.GetItem(Index: Integer): TctlControl;
begin
  Result := inherited Items[Index] as TctlControl;
end;

function TctlControls.Check(AControl: TctlControl): Boolean;
begin
  Result := True;
end;

function TctlControls.Find(const vName: string): TctlControl;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if vName = Items[i].Name then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TctlControls.Add(AControl: TctlControl): Boolean;
begin
  Result := Check(AControl);
  if Result then
    Inherited Add(AControl);
end;

function TctlControls.Add(AControlClass: TctlControlClass): Boolean;
begin
  Result := Add(AControlClass.Create);
end;

function TctlControls.Add(AName: string; ACode: TsardControl): TctlControl;
begin
  Result := TctlControl.Create;
  Result.Name := AName;
  Result.Code := ACode;
  Add(Result);
end;

function TctlControls.Scan(const vText: string; vIndex: Integer): TctlControl;
var
  i: Integer;
  max: Integer;
begin
  Result := nil;
  max := 0;
  for i := 0 to Count -1 do
  begin
    if ScanCompare(Items[i].Name, vText, vIndex) then
    begin
      if max < length(Items[i].Name) then
      begin
        max := length(Items[i].Name);
        Result := Items[i];
      end;
    end;
  end;
end;

function TctlControls.IsOpenBy(const C: Char): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count-1 do
  begin
    if Items[i].Name[1] = LowerCase(C) then
    begin
      Result := True;
      break;
    end;
  end;
end;

{ TsrdObjectList }

constructor TsrdObjectList.Create(AParent: TsoBlock);
begin
  inherited Create;
  FParent := AParent;
end;

{ TsoParentedObject }

procedure TsoParentedObject.SetParent(AValue: TsoBlock);
begin
  if FParent <> nil then
    RaiseError('Already have a parent');
  FParent :=AValue;
end;

{ TrunData }

function TrunData.RegisterID(vName: string): Integer;
begin
  FCount := FCount + 1;
  Result := FCount;
end;

{ TsoSection }

procedure TsoSection.BeforeExecute(vStack: TrunStack; AOperator: TopOperator);
begin
  inherited;
  vStack.Push; //<--here we can push a variable result or create temp result to drop it
  vStack.Scope.Push;
end;

procedure TsoSection.AfterExecute(vStack: TrunStack; AOperator: TopOperator);
var
  T: TrunStackItem;
begin
  inherited;
  T := vStack.Pull;
  if T.Result.AnObject <> nil then
    T.Result.AnObject.Execute(vStack, AOperator);
  FreeAndNil(T);
  vStack.Scope.Pop;
end;

{ TsoNamedObject }

procedure TsoNamedObject.SetName(AValue: string);
begin
  if AValue <> FName then
  begin
    FName := AValue;
  end;
end;

procedure TsoNamedObject.SetID(AValue: Integer);
begin
  if FID =AValue then Exit;
  FID :=AValue;
end;

{ TsoNamedBlock }

procedure TsoNamedBlock.SetName(AValue: string);
begin
  if AValue <> FName then
  begin
    FName := AValue;
  end;
end;

procedure TsoNamedBlock.SetID(AValue: Integer);
begin
  if FID = AValue then Exit;
  FID :=AValue;
end;

{ TrunResult }

procedure TrunResult.SetAnObject(AValue: TsoObject);
begin
  if FAnObject <> AValue then
  begin
    if FAnObject <> nil then
      FreeAndNil(FAnObject);
    FAnObject := AValue;
  end;
end;

destructor TrunResult.Destroy;
begin
  FreeAndNil(FAnObject);
  inherited Destroy;
end;

function TrunResult.HasValue: Boolean;
begin
  Result := AnObject <> nil;
end;

procedure TrunResult.Assign(AResult: TrunResult);
begin
  if AResult.AnObject = nil then
    AnObject := nil
  else
    AnObject := AResult.AnObject.Clone;
end;

function TrunResult.Extract: TsoObject;
begin
  Result := FAnObject;
  FAnObject := nil;
end;

{ TrunScopeItem }

constructor TrunScopeItem.Create;
begin
  inherited Create;
  FVariables := TrunVariables.Create;
end;

destructor TrunScopeItem.Destroy;
begin
  FreeAndNil(FVariables);
  inherited Destroy;
end;

{ TsoConstObject }

function TsoConstObject.DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean;
begin
  if (vStack.Current.Result.AnObject = nil) and (AOperator = nil) then
  begin
    vStack.Current.Result.AnObject := Clone;
    Result := True;
  end
  else
  begin
    if vStack.Current.Result.AnObject = nil then
      vStack.Current.Result.AnObject := Clone(False);
    Result := vStack.Current.Result.AnObject.Operate(Self, AOperator);
  end;
end;

{ TrunStackItem }

procedure TrunStackItem.SetReference(AValue: TrunResult);
begin
  if FReference =AValue then Exit;
  if FReference <> nil then
    RaiseError('Already set a reference');
  FReference :=AValue;
end;

constructor TrunStackItem.Create;
begin
  inherited;
  FResult := TrunResult.Create;
end;

destructor TrunStackItem.Destroy;
begin
  FreeAndNil(FResult);
  inherited Destroy;
end;

{ TrunStack }

function TrunStack.GetCurrent: TrunStackItem;
begin
  Result := (inherited GetCurrent) as TrunStackItem;
end;

function TrunStack.GetParent: TrunStackItem;
begin
  Result := (inherited GetParent) as TrunStackItem;
end;

constructor TrunStack.Create;
begin
  inherited;
  FData := TrunData.Create;
  FScope := TrunScope.Create;
end;

destructor TrunStack.Destroy;
begin
  FreeAndNil(FData);
  FreeAndNil(FScope);
  inherited;
end;

procedure TrunStack.Push(vObject: TrunStackItem);
begin
  inherited Push(vObject);
end;

function TrunStack.Push: TrunStackItem;
begin
  Result := TrunStackItem.Create;
  Push(Result);
end;

function TrunStack.Pull: TrunStackItem;
begin
  Result := (inherited Pull) as TrunStackItem;
end;

{ TsoBlock }

procedure TsoBlock.Created;
begin
  inherited Created;
  FObjectType := otBlock;
end;

function TsoBlock.DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean;
begin
  Result := Items.Execute(vStack);
end;

constructor TsoBlock.Create;
begin
  inherited Create;
  FItems := TsrdBlock.Create(Self);
end;

destructor TsoBlock.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

{ TsardVariables }

function TrunVariables.GetItem(Index: Integer): TrunVariable;
begin
  Result := inherited Items[Index] as TrunVariable;
end;

function TrunVariables.Find(vName: string): TrunVariable;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vName, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TrunVariables.Register(vName: string): TrunVariable;
begin
  Result := Find(vName);
  if Result = nil then
  begin
    Result := TrunVariable.Create;
    Result.Name := vName;
    Add(Result);
  end;
end;

function TrunVariables.SetValue(vName: string; vValue: TsoObject): TrunVariable;
begin
  Result := Find(vName);
  if Result <> nil then
    Result.Value.AnObject := vValue;
end;

{ TrunVariable }

procedure TrunVariable.SetName(AValue: string);
begin
  if FName =AValue then Exit;
  FName :=AValue;
end;

constructor TrunVariable.Create;
begin
  inherited Create;
  Value := TrunResult.Create;
end;

destructor TrunVariable.Destroy;
begin
  FreeAndNil(Value);
  inherited Destroy;
end;

{ TsrdBlockStack }

function TsrdBlockStack.GetCurrent: TsrdBlock;
begin
  Result := (inherited GetCurrent) as TsrdBlock;
end;

procedure TsrdBlockStack.Push(vItem: TsrdBlock);
begin
  inherited Push(vItem);
end;

{ TopNot }

constructor TopNot.Create;
begin
  inherited Create;
  Name := '!'; //or '~'
  Title := 'Not';
  Level := 100;
end;

{ TopOr }

constructor TopOr.Create;
begin
  inherited Create;
  Name := '|';
  Title := 'Or';
  Level := 51;
end;

{ TopAnd }

constructor TopAnd.Create;
begin
  inherited Create;
  Name := '&';
  Title := 'And';
  Level := 51;
end;

{ TopNotEqual }

constructor TopNotEqual.Create;
begin
  inherited Create;
  Name := '<>';
  Title := 'NotEqual';
  Level := 51;
end;

{ TopEqual }

constructor TopEqual.Create;
begin
  inherited Create;
  Name := '=';
  Title := 'Equal';
  Level := 51;
end;

{ TopGreater }

constructor TopGreater.Create;
begin
  inherited Create;
  Name := '>';
  Title := 'Greater';
  Level := 51;
end;

{ TopLesser }

constructor TopLesser.Create;
begin
  inherited Create;
  Name := '<';
  Title := 'Lesser';
  Level := 51;
end;

{ TopPower }

constructor TopPower.Create;
begin
  inherited Create;
  Name := '^';
  Title := 'Power';
  Level := 52;
end;

procedure TsoAssign.Created;
begin
  inherited Created;
  FObjectType := otVariable;
end;

function TsoAssign.DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean;
var
  v: TrunVariable;
begin
  { if not name it assign to parent result }
  Result := True;
  if Name = '' then
    vStack.Current.Reference := vStack.Parent.Result
  else
  begin
    v := vStack.Scope.Current.Variables.Register(Name);
    if v <> nil then
    begin
      vStack.Current.Reference := v.Value;
    end;
  end;
end;

{ TopDivide }

constructor TopDivide.Create;
begin
  inherited Create;
  Name := '/';
  Title := 'Divition';
  Level := 51;
end;

{ TopMultiply }

constructor TopMultiply.Create;
begin
  inherited Create;
  Name := '*';
  Title := 'Multiply';
  Level := 51;
end;

{ TopMinus }

constructor TopMinus.Create;
begin
  inherited Create;
  Name := '-';
  Title := 'Minus';
  Level := 50;
  Description := 'Sub object to another object';
end;

{ TopOperator }

function TopOperator.DoExecute(vStack: TrunStack; vObject: TsoObject): Boolean;
begin
  Result := False;
end;

function TopOperator.Execute(vStack: TrunStack; vObject: TsoObject): Boolean;
begin
  Result := False;
end;

constructor TopOperator.Create;
begin
  inherited Create;
end;

function TopOperators.Find(const vName: string): TopOperator;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if vName = Items[i].Name then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TopOperators.FindByTitle(const vTitle: string): TopOperator;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if vTitle = Items[i].Title then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TopOperators.Add(AOperator: TopOperator): Boolean;
begin
  Result := Check(AOperator);
  if Result then
    inherited Add(AOperator);
end;

function TopOperators.Add(AOperatorClass: TopOperatorClass): Boolean;
begin
  Result := Add(AOperatorClass.Create);
end;

function TopOperators.IsOpenBy(const C: Char): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count-1 do
  begin
    if Items[i].Name[1] = LowerCase(C) then
    begin
      Result := True;
      break;
    end;
  end;
end;

function TopOperators.Scan(const vText: string; vIndex: Integer): TopOperator;
var
  i: Integer;
  max: Integer;
begin
  Result := nil;
  max := 0;
  for i := 0 to Count -1 do
  begin
    if ScanCompare(Items[i].Name, vText, vIndex) then
    begin
      if max < length(Items[i].Name) then
      begin
        max := length(Items[i].Name);
        Result := Items[i];
      end;
    end;
  end;
end;

{ TopPlus }

constructor TopPlus.Create;
begin
  inherited Create;
  Name := '+';
  Title := 'Plus';
  Level := 50;
  Description := 'Add object to another object';
end;

{ TopOperators }

function TopOperators.GetItem(Index: Integer): TopOperator;
begin
  Result := inherited Items[Index] as TopOperator;
end;

function TopOperators.Check(AOperator: TopOperator): Boolean;
begin
  Result := True;
end;

{ TsrdEngine }

procedure TsrdEngine.Created;
begin
  inherited;
end;

constructor TsrdEngine.Create;
begin
  inherited Create;
end;

{ TsrdStatementItem }

function TsrdStatementItem.Execute(vStack: TrunStack): Boolean;
begin
  if AnObject = nil then
    RaiseError('Object not set!');
  Result := AnObject.Execute(vStack, AnOperator);
end;

{ TsrdClass }

procedure TsoDeclare.Created;
begin
  inherited Created;
  FObjectType := otClass;
end;

function TsoDeclare.DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean;
begin
  Result :=inherited Execute(vStack, AOperator);
end;

{ TsrdInstance }

procedure TsoInstance.Created;
begin
  inherited Created;
  FObjectType := otObject;
end;

procedure TsoInstance.FindMe;
begin

end;

function TsoInstance.DoExecute(vStack: TrunStack; AOperator: TopOperator): Boolean;
var
  v: TrunVariable;
begin
  v := vStack.Scope.Current.Variables.Register(Name);//TODO find it not register it
  if v <> nil then
  begin
    if v.Value.AnObject = nil then
      RaiseError(v.Name + ' variable have no value yet');//TODO make it as empty
    Result := v.Value.AnObject.Execute(vStack, AOperator);
  end
  else
    Result := False;
end;

{ TsrdBoolean }

procedure TsoBoolean.Created;
begin
  inherited Created;
  FObjectType := otBoolean;
end;

function TsoBoolean.ToString(out outValue: string): Boolean;
begin
  Result :=inherited ToString(outValue);
end;

function TsoBoolean.ToFloat(out outValue: Float): Boolean;
begin
  Result :=inherited ToFloat(outValue);
end;

function TsoBoolean.ToInteger(out outValue: int): Boolean;
begin
  Result :=inherited ToInteger(outValue);
end;

function TsoBoolean.ToBoolean(out outValue: Boolean): Boolean;
begin
  outValue := Value;
  Result := True;
end;

{ TsrdString }

procedure TsoString.Created;
begin
  inherited Created;
  FObjectType := otString;
end;

procedure TsoString.Assign(FromObject: TsoObject);
begin
  if FromObject <> nil then
  begin
    if FromObject is TsoString then
      Value := (FromObject as TsoString).Value
    else
      Value := FromObject.AsString;
  end;
end;

function TsoString.Operate(AObject: TsoObject; AOperator: TopOperator): Boolean;
begin
  Result := True;
  case AOperator.Name of
    '+': Value := Value + AObject.AsString;
    '-':
      begin
        if AObject is TsoNumber then
          Value := LeftStr(Value, Length(Value) - AObject.AsInteger)
        else
          Result := False;
      end;
    '*':
      begin
        if AObject is TsoNumber then
          Value := StringRepeat(Value, AObject.AsInteger)
        else
          Result := False;
      end;
//    '/': Value := Value / AObject.AsString;
    else
      Result := False;
  end;
end;

function TsoString.ToString(out outValue: string): Boolean;
begin
  outValue := Value;
  Result := True;
end;

function TsoString.ToFloat(out outValue: Float): Boolean;
begin
  outValue := StrToFloat(Value);
  Result := True;
end;

function TsoString.ToInteger(out outValue: int): Boolean;
begin
  outValue := StrToInt64(Value);
  Result := True;
end;

function TsoString.ToBoolean(out outValue: Boolean): Boolean;
begin
  if not TryStrToBool(Value, outValue) then
    outValue := AsInteger <> 0;
  Result := True;
end;

{ TsrdFloat }

procedure TsoFloat.Created;
begin
  inherited Created;
  FObjectType := otFloat;
end;

procedure TsoFloat.Assign(FromObject: TsoObject);
begin
  if FromObject <> nil then
  begin
    if FromObject is TsoFloat then
      Value := (FromObject as TsoFloat).Value
    else
      Value := FromObject.AsFloat;
  end;
end;

function TsoFloat.Operate(AObject: TsoObject; AOperator: TopOperator): Boolean;
begin
  Result := True;
  WriteLn(FloatToStr(Value) + ' '+ AOperator.Name + ' ' +IntToStr(AObject.AsInteger));
  case AOperator.Name of
    '+': Value := Value + AObject.AsFloat;
    '-': Value := Value - AObject.AsFloat;
    '*': Value := Value * AObject.AsFloat;
    '/': Value := Value / AObject.AsFloat;
    else
      Result := False;
  end;
end;

function TsoFloat.ToString(out outValue: string): Boolean;
begin
  outValue := FloatToStr(Value);
  Result := True;
end;

function TsoFloat.ToFloat(out outValue: Float): Boolean;
begin
  outValue := Value;
  Result := True;
end;

function TsoFloat.ToInteger(out outValue: int): Boolean;
begin
  outValue := round(Value);
  Result := True;
end;

function TsoFloat.ToBoolean(out outValue: Boolean): Boolean;
begin
  outValue := Value <> 0;
  Result := True;
end;

{ TsrdInteger }

procedure TsoInteger.Created;
begin
  inherited Created;
  FObjectType := otInteger;
end;

constructor TsoInteger.Create(AValue: int);
begin
  inherited Create;
  Value := AValue;
end;

procedure TsoInteger.Assign(FromObject: TsoObject);
begin
  if FromObject <> nil then
  begin
    if FromObject is TsoInteger then
      Value := (FromObject as TsoInteger).Value
    else
      Value := FromObject.AsInteger;
  end;
end;

function TsoInteger.Operate(AObject: TsoObject; AOperator: TopOperator): Boolean;
begin
  Result := True;
  WriteLn(IntToStr(Value) + ' ' + AOperator.Name + ' ' +IntToStr(AObject.AsInteger));
  case AOperator.Name of
    '+': Value := Value + AObject.AsInteger;
    '-': Value := Value - AObject.AsInteger;
    '*': Value := Value * AObject.AsInteger;
    '/': Value := Value div AObject.AsInteger;
    else
      Result := False;
  end;
end;

function TsoInteger.ToString(out outValue: string): Boolean;
begin
  Result := True;
  outValue := IntToStr(Value);
end;

function TsoInteger.ToFloat(out outValue: Float): Boolean;
begin
  Result := True;
  outValue := Value;
end;

function TsoInteger.ToInteger(out outValue: int): Boolean;
begin
  Result := True;
  outValue := Value;
end;

function TsoInteger.ToBoolean(out outValue: Boolean): Boolean;
begin
  outValue := Value <> 0;
  Result := True;
end;

{ TsoBlock }

function TsrdBlock.GetStatement: TsrdStatement;
begin
  //Check;//TODO: not sure
  Result := Last as TsrdStatement;
end;

function TsrdBlock.GetItem(Index: Integer): TsrdStatement;
begin
  Result := inherited Items[Index] as TsrdStatement;
end;

function TsrdBlock.Add(AStatement: TsrdStatement): Integer;
begin
  Result := inherited Add(AStatement);
end;

function TsrdBlock.Add: TsrdStatement;
begin
  Result := TsrdStatement.Create(Parent);
  Add(Result);
end;

procedure TsrdBlock.Check;
begin
  if Count = 0  then
    Add;
end;

function TsrdBlock.Execute(vStack: TrunStack): Boolean;
var
  i: Integer;
begin
  Result := Count > 0;
  for i := 0 to Count -1 do
  begin
    vStack.Push;

    Result := Items[i].Execute(vStack) and Result;

    if vStack.Current.Reference <> nil then
      vStack.Current.Reference.AnObject := vStack.Current.Result.Extract;  //it is responsible of assgin to parent result or to variable

    vStack.Pop;
  end;
end;

{ TsrdStatement }

function TsrdStatement.GetItem(Index: Integer): TsrdStatementItem;
begin
  Result := inherited Items[Index] as TsrdStatementItem;
end;

procedure TsrdStatement.SetDebug(AValue: TsrdDebug);
begin
  if FDebug =AValue then Exit;
  FDebug :=AValue;
end;

function TsrdStatement.Add(AObject: TsrdStatementItem): Integer;
begin
  Result := inherited Add(AObject);
end;

function TsrdStatement.Add(AOperator: TopOperator; AObject: TsoObject): TsrdStatementItem;
begin
  Result := TsrdStatementItem.Create;
  Result.AnOperator := AOperator;
  Result.AnObject := AObject;
  if AObject is TsoParentedObject then
    (AObject as TsoParentedObject).Parent := Parent;
  Add(Result);
end;

function TsrdStatement.Execute(vStack: TrunStack): Boolean;
var
  i: Integer;
begin
  Result := Count > 0;
  for i := 0 to Count -1 do
    Result := Items[i].Execute(vStack) and Result;
end;

{ TsoRun }

constructor TsoMain.Create;
begin
  inherited Create;
end;

destructor TsoMain.Destroy;
begin
  inherited Destroy;
end;

{ TsrdObjects }

function TsrdObjects.GetItem(Index: Integer): TsoObject;
begin
  Result := inherited Items[Index] as TsoObject;
end;

{ TsoObject }

constructor TsoObject.Create;
begin
  inherited Create;
  Writeln('->'+ClassName);
end;

function TsoObject.This: TsoObject;
begin
  Result := Self;
end;

function TsoObject.Operate(AObject: TsoObject; AOperator: TopOperator): Boolean;
begin
  Result := False;
end;

procedure TsoObject.BeforeExecute(vStack: TrunStack; AOperator: TopOperator);
begin
end;

procedure TsoObject.AfterExecute(vStack: TrunStack; AOperator: TopOperator);
begin

end;

function TsoObject.Execute(vStack: TrunStack; AOperator: TopOperator): Boolean;
var
  s: string;
begin
  Result := False;
  BeforeExecute(vStack, AOperator);
  Result := DoExecute(vStack, AOperator);
  AfterExecute(vStack, AOperator);

  Result := True;//Nothing to do
  s := StringOfChar('-', vStack.CurrentItem.Level)+'->';
  s := s + 'Execute: ' + ClassName+ ' Level=' + IntToStr(vStack.CurrentItem.Level);
  if AOperator <> nil then
    s := s +'{'+ AOperator.ClassName+'}';
  WriteLn(s);
end;

procedure TsoObject.Assign(FromObject: TsoObject);
begin
  //Nothing to do
end;

function TsoObject.Clone(WithValue: Boolean): TsoObject;
begin
  Result := TsoObjectClass(ClassType).Create;
  if WithValue then
    Result.Assign(Self);
end;

function TsoObject.GetAsString: String;
var
  o: string;
begin
  if ToString(o) then
    Result := o
  else
    Result := '';
end;

function TsoObject.GetAsFloat: Float;
var
  o: Float;
begin
  if ToFloat(o) then
    Result := o
  else
    Result := 0;
end;

function TsoObject.GetAsInteger: int;
var
  o: int;
begin
  if ToInteger(o) then
    Result := o
  else
    Result := 0;
end;

function TsoObject.GetAsBoolean: Boolean;
var
  o: Boolean;
begin
  if ToBoolean(o) then
    Result := o
  else
    Result := False;
end;

function TsoObject.ToBoolean(out outValue: Boolean): Boolean;
begin
  Result := False;
end;

function TsoObject.ToString(out outValue: string): Boolean;
begin
  Result := False;
end;

function TsoObject.ToFloat(out outValue: Float): Boolean;
begin
  Result := False;
end;

function TsoObject.ToInteger(out outValue: int): Boolean;
begin
  Result := False;
end;

end.
