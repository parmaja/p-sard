unit sardObjects;
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

{*TODO:
create variable manually in the example to test return value in formual
10+x

soVariable find it is value in the stack and use it in formual

soVariable create a runtime variable in the stack
x := 10;
}

{
  Prefix guid:
    srd: global classes inherited from sard
    run: Runtime classes
    so: Sard objects, that created when compile the source
    op: Operators objects
}

interface

uses
  Classes, SysUtils,
  sardClasses;

const
  sSardVersion = '0.1';
  iSardVersion = 010;

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
  TsrdDefines = class;

  TsrdDefine = class(TsardObject)
  public
    Name: string;
    ResultType: string;
  end;

  TsrdDefineParams = class(specialize GsardNamedObjects<TsrdDefine>)
  end;

  { TsrdDefines }

  TsrdDefines = class(specialize GsardNamedObjects<TsrdDefine>)
  private
    FParams: TsrdDefineParams;
  public
    constructor Create;
    destructor Destroy; override;
    property Params: TsrdDefineParams read FParams;
  end;

  { TsrdStatementItem }

  TsrdStatementItem = class(TsardObject)
  public
    anOperator: TopOperator;
    anObject: TsoObject;
    function Execute(vStack: TrunStack): Boolean;
  end;

  { TsrdObjectList }

  TsrdObjectList = class(TsardObjectList)
  private
    FParent: TsoObject;
  public
    constructor Create(AParent: TsoObject);
    property Parent: TsoObject read FParent;
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

  TsoClass = class;
  TsoDeclare = class;

  { TclsClasses }

  TclsClasses = class(specialize GsardNamedObjects<TsoClass>)
  private
    function GetItem(Index: Integer): TsoClass;
  public
    function Add(vName: string; vDeclare: TsoDeclare): TsoClass; overload;
    function Add(vClass: TsoClass): Integer;
  end;

  { TsoObject }

  TsoObject = class abstract(TsardObject, IsrdObject)
  private
    FParent: TsoObject;
  protected
    FObjectType: TsrdObjectType;

    function GetAsString: String;
    function GetAsFloat: Float;
    function GetAsInteger: int;
    function GetAsBoolean: Boolean;

    function Operate(AObject: TsoObject; AOperator: TopOperator): Boolean; virtual;
    procedure BeforeExecute(vStack: TrunStack; AOperator: TopOperator); virtual;
    procedure AfterExecute(vStack: TrunStack; AOperator: TopOperator); virtual;
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); virtual; abstract;
    procedure DoSetParent(AValue: TsoObject); virtual;
    procedure SetParent(AValue: TsoObject);
  public
    constructor Create; virtual;
    function This: TsoObject; //return the same object, stupid but save some code :P
    function Execute(vStack: TrunStack; AOperator: TopOperator): Boolean;
    procedure Assign(FromObject: TsoObject); virtual;
    function Clone(WithValue: Boolean = True): TsoObject; virtual;


    property Parent: TsoObject read FParent write SetParent;
    function AddClass(vName: string; vDeclare: TsoDeclare): TsoClass; virtual;
    function FindClass(vName: string): TsoObject; virtual;

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

  TsrdObjects = class(specialize GsardObjects<TsoObject>)
  private
  public
  end;

  {-------- Objects --------}

  { TsoConstObject }

  TsoConstObject = class abstract(TsoObject)
  protected
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override; final;
  public
  end;

  { TsoBlock }

  TsoBlock = class abstract(TsoObject, IsrdBlock)
  protected
    FBlock: TsrdBlock;
    procedure Created; override;
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Block: TsrdBlock read FBlock;
  end;

  { TsoSection }
  (* Used by { } *)

  TsoSection = class(TsoBlock) //Result of was droped until using := assign in the first of statment
  private
    FClasses: TclsClasses;
  protected
    procedure BeforeExecute(vStack: TrunStack; AOperator: TopOperator); override;
    procedure AfterExecute(vStack: TrunStack; AOperator: TopOperator); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function FindClass(vName: string): TsoObject; override;
    function AddClass(vName: string; vDeclare: TsoDeclare): TsoClass; override;
    //procedure AddClass(vClass: TsoClass); override;
    property Classes: TclsClasses read FClasses; //It is cache of object listed inside statments, it is for fast find the object
  end;

  { TsoCustomStatement }

  TsoCustomStatement = class(TsoObject)
  private
  protected
    FStatement: TsrdStatement;
    procedure BeforeExecute(vStack: TrunStack; AOperator: TopOperator); override;
    procedure AfterExecute(vStack: TrunStack; AOperator: TopOperator); override;
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
    property Statement: TsrdStatement read FStatement;
  end;

  { TsoStatement }

  TsoStatement = class(TsoCustomStatement)
  private
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TsoClass }

  TsoClass = class(TsoCustomStatement)
  private
    FName: string;
    FDeclare: TsoDeclare;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetDeclare(ADeclare: TsoDeclare);
    property Name: string read FName write FName;
  end;

  { TsoNamedObject }

  TsoNamedObject = class(TsoObject)
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

  TsoInstance = class(TsoNamedBlock)
  private
  protected
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
    procedure Created; override;
  end;


  { TsoParam }

  TsoParam = class(TsoNamedObject)
  private
  protected
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
  end;

  { TsoAssign }

  { It is assign a variable value, x:=10 + y}

  TsoAssign = class(TsoNamedObject)
  private
  protected
    procedure Created; override;
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
  end;

  TprmParam = class(TsardObject)
  public
    Name: string;
    ParamType: string; //TODO must be object
  end;

  { TprmParams }

  TprmParams = class(specialize GsardObjects<TprmParam>)
  end;

  { TsoDeclare }

  TsoDeclare = class(TsoNamedObject)
  private
    FDefines: TsrdDefines;
    FStatement: TsrdStatement;
    procedure SetStatement(AValue: TsrdStatement);
  protected
    procedure Created; override;
    procedure DoSetParent(AValue: TsoObject); override;
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Defines: TsrdDefines read FDefines;
    property Statement: TsrdStatement read FStatement write SetStatement;
  end;

{-------- Const Objects --------}

  { TsrdNone }

  TsoNone = class(TsoConstObject) //None it is not Null, it is an initial value we sart it
  public
    //Do operator
    //Convert to 0 or ''
  end;

  { TsrdNumber }

  TsoNumber = class abstract(TsoConstObject)
  public
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

  { TsoFloat }

  TsoFloat = class(TsoNumber)
  public
    Value: Float;
    constructor Create(AValue: Float); overload;
    procedure Created; override;
    procedure Assign(FromObject: TsoObject); override;
    function Operate(AObject: TsoObject; AOperator: TopOperator): Boolean; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsoString }

  TsoString = class(TsoConstObject)
  public
    Value: string;
    constructor Create(AValue: string); overload;
    procedure Created; override;
    procedure Assign(FromObject: TsoObject); override;
    function Operate(AObject: TsoObject; AOperator: TopOperator): Boolean; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

  { TsoBoolean }

  TsoBoolean = class(TsoNumber)
  public
    Value: Boolean;
    constructor Create(AValue: Boolean); overload;
    procedure Created; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;

{
  TODO:

  TsoArray = class(TsoNumber)
  public
    Value: TsoObjects;
    constructor Create(AValue: Boolean); overload;
    procedure Created; override;
    function ToString(out outValue: string): Boolean; override;
    function ToFloat(out outValue: Float): Boolean; override;
    function ToInteger(out outValue: int): Boolean; override;
    function ToBoolean(out outValue: Boolean): Boolean; override;
  end;}

  //TsoDateTime =
  //TsoColor

  { TsoComment }

  TsoComment = class(TsoObject)
  protected
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
    Value: string;
    constructor Create(AValue: string); overload;
    procedure Created; override;
  end;

{  TsoPreprocessor = class(TsoObject)
  protected
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
    Value: string;
    constructor Create(AValue: string); overload;
    procedure Created; override;
  end;}

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

  TctlControls = class(specialize GsardNamedObjects<TctlControl>)
  private
  protected
  public
    function Add(AControlClass: TctlControlClass): Integer; overload;
    function Add(AName: string; ACode: TsardControl): TctlControl; overload;
    function Scan(const vText: string; vIndex: Integer): TctlControl;
    function IsOpenBy(const C: Char): Boolean;
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

  TopOperators = class(specialize GsardNamedObjects<TopOperator>)
  private
  protected
  public
    function FindByTitle(const vTitle: string): TopOperator;
    function Add(AOperatorClass: TopOperatorClass): Integer; overload;
    function IsOpenBy(const C: Char): Boolean;
    function Scan(const vText: string; vIndex: Integer): TopOperator;
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

  TrunVariables = class(specialize GsardNamedObjects<TrunVariable>)
  private
  public
    function Register(vName: string): TrunVariable;
    function SetValue(vName: string; vValue: TsoObject): TrunVariable;
  end;

  { TrunResult }

  TrunResult = class(TsardObject)
  private
    FanObject: TsoObject;
    procedure SetanObject(AValue: TsoObject);
  public
    destructor Destroy; override;
    function HasValue: Boolean;
    procedure Assign(AResult: TrunResult); virtual;
    function Extract: TsoObject;
    property anObject: TsoObject read FanObject write SetanObject;
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

  {----------------------------------}

  { TsoLog_Proc }

  TsoLog_Proc = class(TsoNamedObject)
  protected
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
  end;

  { TsoVersion_Const }

  TsoVersion_Const = class(TsoNamedObject)
  protected
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
  end;

  { TsoTime_Const }

  TsoTime_Const = class(TsoNamedBlock)
  protected
    procedure DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean); override;
  public
  end;

  { TsoMain }

  TsoMain = class(TsoSection)
  public
    LogProc: TsoLog_Proc; //for test
    VersionProc: TsoVersion_Const;
    constructor Create; override;
    destructor Destroy; override;
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

{ TsrdDefines }

constructor TsrdDefines.Create;
begin
  inherited Create;
  FParams := TsrdDefineParams.Create;
end;

destructor TsrdDefines.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

{ TsoCustomStatement }

procedure TsoCustomStatement.BeforeExecute(vStack: TrunStack; AOperator: TopOperator);
begin
  vStack.Push;
end;

procedure TsoCustomStatement.AfterExecute(vStack: TrunStack; AOperator: TopOperator);
var
  T: TrunStackItem;
begin
  inherited;
  T := vStack.Pull;
  if T.Result.anObject <> nil then
    T.Result.anObject.Execute(vStack, AOperator);
  FreeAndNil(T);
end;

procedure TsoCustomStatement.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
begin
  Done := FStatement.Execute(vStack);
end;

constructor TsoClass.Create;
begin
  inherited Create;
end;

destructor TsoClass.Destroy;
begin
  inherited Destroy;
end;

procedure TsoClass.SetDeclare(ADeclare: TsoDeclare);
begin
  if FDeclare <> nil then
    RaiseError('Declare is already set!');
  FDeclare := ADeclare;
  FStatement := ADeclare.Statement;
end;

{ TsoParam }

procedure TsoParam.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
begin
end;

{ TclsClasses }

function TclsClasses.GetItem(Index: Integer): TsoClass;
begin
  Result := inherited GetItem(Index) as TsoClass;
end;

function TclsClasses.Add(vName: string; vDeclare: TsoDeclare): TsoClass;
begin
  Result := TsoClass.Create;
  Result.Name := vName;
  Result.SetDeclare(vDeclare);
  Result.Parent := vDeclare;
  Add(Result);
end;

function TclsClasses.Add(vClass: TsoClass): Integer;
begin
  Result := inherited Add(vClass);
end;

{ TsoTime_Const }

procedure TsoTime_Const.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
begin
  inherited;;
  vStack.Current.Result.anObject := TsoString.Create(TimeToStr(Time));
end;

{ TsoVersion_Const }

procedure TsoVersion_Const.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
begin
  vStack.Current.Result.anObject := TsoString.Create(sSardVersion);
end;

{ TsoLog_Proc }

procedure TsoLog_Proc.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
begin
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

{ TsoComment }

procedure TsoComment.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
begin
  Done := True;
end;

constructor TsoComment.Create(AValue: string);
begin
  Create;
  Value := AValue;
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

function TctlControls.Add(AControlClass: TctlControlClass): Integer;
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

constructor TsrdObjectList.Create(AParent: TsoObject);
begin
  inherited Create;
  FParent := AParent;
end;

procedure TsoObject.SetParent(AValue: TsoObject);
begin
  if FParent <> nil then
    RaiseError('Already have a parent');
  FParent :=AValue;
  DoSetParent(FParent);
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
  if T.Result.anObject <> nil then
    T.Result.anObject.Execute(vStack, AOperator);
  FreeAndNil(T);
  vStack.Scope.Pop;
end;

constructor TsoSection.Create;
begin
  inherited;
  FClasses := TclsClasses.Create;
end;

destructor TsoSection.Destroy;
begin
  FreeAndNil(FClasses);
  inherited Destroy;
end;

function TsoSection.AddClass(vName: string; vDeclare: TsoDeclare): TsoClass;
begin
  Result := Classes.Add(vName, vDeclare);
end;

function TsoSection.FindClass(vName: string): TsoObject;
begin
  Result := Classes.Find(vName);
  if Result = nil then
    Result := inherited FindClass(vName);
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

procedure TrunResult.SetanObject(AValue: TsoObject);
begin
  if FanObject <> AValue then
  begin
    if FanObject <> nil then
      FreeAndNil(FanObject);
    FanObject := AValue;
  end;
end;

destructor TrunResult.Destroy;
begin
  FreeAndNil(FanObject);
  inherited Destroy;
end;

function TrunResult.HasValue: Boolean;
begin
  Result := anObject <> nil;
end;

procedure TrunResult.Assign(AResult: TrunResult);
begin
  if AResult.anObject = nil then
    anObject := nil
  else
    anObject := AResult.anObject.Clone;
end;

function TrunResult.Extract: TsoObject;
begin
  Result := FanObject;
  FanObject := nil;
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

procedure TsoConstObject.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
begin
  if (vStack.Current.Result.anObject = nil) and (AOperator = nil) then
  begin
    vStack.Current.Result.anObject := Clone;
    Done := True;
  end
  else
  begin
    if vStack.Current.Result.anObject = nil then
      vStack.Current.Result.anObject := Clone(False);
    Done := vStack.Current.Result.anObject.Operate(Self, AOperator);
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

procedure TsoBlock.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
begin
  Done := Block.Execute(vStack);
end;

constructor TsoBlock.Create;
begin
  inherited Create;
  FBlock := TsrdBlock.Create(Self);
end;

destructor TsoBlock.Destroy;
begin
  FreeAndNil(FBlock);
  inherited Destroy;
end;

{ TsardVariables }

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
    Result.Value.anObject := vValue;
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

procedure TsoAssign.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
var
  v: TrunVariable;
begin
  { if not name it assign to parent result }
  Done := True;
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

function TopOperators.Add(AOperatorClass: TopOperatorClass): Integer;
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
  if anObject = nil then
    RaiseError('Object not set!');
  Result := anObject.Execute(vStack, anOperator);
end;

{ TsrdClass }

procedure TsoDeclare.SetStatement(AValue: TsrdStatement);
begin
  if FStatement =AValue then Exit;
  FStatement :=AValue;
end;

procedure TsoDeclare.Created;
begin
  inherited Created;
  FObjectType := otClass;
end;

procedure TsoDeclare.DoSetParent(AValue: TsoObject);
begin
  AddClass(Name, Self);
end;

procedure TsoDeclare.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
begin
end;

constructor TsoDeclare.Create;
begin
  inherited;
  FStatement := TsrdStatement.Create(Self);
  FDefines := TsrdDefines.Create;
end;

destructor TsoDeclare.Destroy;
begin
  FreeAndNil(FDefines);
  FreeAndNil(FStatement);
  inherited Destroy;
end;

{ TsrdInstance }

procedure TsoInstance.Created;
begin
  inherited Created;
  FObjectType := otObject;
end;

procedure TsoInstance.DoExecute(vStack: TrunStack; AOperator: TopOperator; var Done: Boolean);
var
  v: TrunVariable;
  p: TsoObject;
begin
  p := FindClass(Name);
  if p <> nil then
  begin
    Done := p.Execute(vStack, AOperator);
  end
  else
  begin
    v := vStack.Scope.Current.Variables.Register(Name);//TODO find it not register it
    if v <> nil then
    begin
      if v.Value.anObject = nil then
        RaiseError(v.Name + ' variable have no value yet');//TODO make it as empty
      Done := v.Value.anObject.Execute(vStack, AOperator);
    end
    else
      Done := False;
  end
end;

{ TsrdBoolean }

constructor TsoBoolean.Create(AValue: Boolean);
begin
  Create;
  Value := AValue;
end;

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

constructor TsoString.Create(AValue: string);
begin
  Create;
  Value := AValue;
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

constructor TsoFloat.Create(AValue: Float);
begin
  inherited Create;
  Value := AValue;
end;

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
      vStack.Current.Reference.anObject := vStack.Current.Result.Extract;  //it is responsible of assgin to parent result or to variable

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
  Result.anOperator := AOperator;
  Result.anObject := AObject;
  AObject.Parent := Parent;
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

constructor TsoMain.Create;
begin
  inherited Create;
  LogProc := TsoLog_Proc.Create;
  LogProc.Parent := Self;
  VersionProc := TsoVersion_Const.Create;
  VersionProc.Parent := Self;
  //Items.Add()
end;

destructor TsoMain.Destroy;
begin
  FreeAndNil(LogProc);
  FreeAndNil(VersionProc);
  inherited Destroy;
end;

{ TsoObject }

constructor TsoObject.Create;
begin
  inherited Create;
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

procedure TsoObject.DoSetParent(AValue: TsoObject);
begin
end;

function TsoObject.Execute(vStack: TrunStack; AOperator: TopOperator): Boolean;
var
  s: string;
begin
  Result := False;
  BeforeExecute(vStack, AOperator);
  DoExecute(vStack, AOperator, Result);
  AfterExecute(vStack, AOperator);

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

function TsoObject.AddClass(vName: string; vDeclare: TsoDeclare): TsoClass;
begin
  if Parent = nil then
    Result := nil
  else
    Result := Parent.AddClass(vName, vDeclare);
end;

function TsoObject.FindClass(vName: string): TsoObject;
begin
  if Parent <> nil then
    Result := Parent.FindClass(vName)
  else
    Result := nil;
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
