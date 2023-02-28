unit sardClasses;
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
  Classes, SysUtils, Contnrs,
  mnClasses, mnUtils;

const
  sEOL = [#0, #13, #10];
  sEscape = '\';

type
  Bool = Boolean;
  Text = string;
  //Integer = Integer;
  Number = Double;

  ESardException = class(Exception)
  private
    FCode: Cardinal;
  public
    property Code: Cardinal read FCode write FCode;
  end;

  { EsardParserException }

  ESardParserException = class(Exception)
  private
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(const Msg: string; const Line, Column : Integer);
    property Column: Integer read FColumn write FColumn;
    property Line: Integer read FLine write FLine;
  end;

  { TSardObject }

  TSardObject = TmnObject;

  TSardString = record
  private
    FStartPos: Integer;
    FEndPos: Integer;
    FValue: string;
  public
    constructor Create(const AValue: String; AStartPos: Integer = 1; AEndPos: Integer = -1);
    property StartPos: Integer read FStartPos;
    property EndPos: Integer read FEndPos;
    property Value: string read FValue;
  end;

  TSourceWriter = class;

  { TSardObjects }

  TSardObjects<_Object_: class> = class(TmnObjectList<_Object_>)
  protected
  public
  end;

  { TSardNamedObject }

  TSardNamedObject = class(TmnNamedObject)
  private
  protected
    procedure ExportWrite(Writer: TSourceWriter; LastOne: Boolean; Level: Integer); virtual;
  public
    function IsOpenBy(C: Char): Boolean;
  end;

  { TsardNamedObjectList }

  TSardNamedObjects<_Object_: TSardNamedObject> = class(TmnNamedObjectList<_Object_>)
  protected
  public
    function Scan(const vText: string; vIndex: Integer): _Object_;
    function IsOpenBy(C: Char): Boolean;
  end;

  TSardStackObject = class(TSardObject)
  public
    Level: Integer;
  end;

  { TsardStack }

  TSardStack<_Node_: TSardObject> = class(TSardObject)
  protected
    Own: Boolean;
    type

      TSardStackItem = class(TSardObject)
      protected
        OwnIt: Boolean;
        Node: _Node_;
        Parent: TSardStackItem;
      public
        Owner: TSardStack<_Node_>;
        Level: Integer;
        procedure SetObject(vNode:  _Node_; OwnIt: Boolean = False);
      end;

  private
    FCount: Integer;
    FTop: TSardStackItem;
  protected
    function GetParent: _Node_;
    function GetCurrent: _Node_;
    procedure AfterPush; virtual;
    procedure BeforePop; virtual;
  public
    constructor Create(OwnItems: Boolean); virtual;
    destructor Destroy; override;
    procedure Clear;
    function IsEmpty: Boolean;
    procedure Push(vObject: _Node_; OwnIt: Boolean = True); overload;
    function Pull: _Node_; //Pop but do not delete the object
    procedure Pop;
    function Peek: _Node_;
    procedure SetCurrent(vNode:  _Node_; OwnIt: Boolean = False);
    property Current: _Node_ read GetCurrent;
    property Parent: _Node_ read GetParent;
    property Top: TSardStackItem read FTop;
    property Count: Integer read FCount;
  end;

  { TSourceWriter }

  TSourceWriter = class abstract(TObject)
  public
    TabWidth: Integer;
    constructor Create;
    procedure Add(const S: string); overload; virtual; abstract;
    procedure Add(Level: Integer = 1; S: string = ''); overload;
    procedure NewLine; virtual; abstract;
  end;

  { TStringSourceWriter }

  TStringSourceWriter = class(TSourceWriter)
  private
    FStrings: TStrings;
    FLine: string;
  public
    constructor Create(Strings: TStrings);
    destructor Destroy; override;
    procedure Flush;
    procedure Add(const S: string); override;
    procedure NewLine; override;
  end;

procedure RaiseError(AError: string; Line: Integer = 0; Column: Integer = 0);

function ScanText1(const S: string; const Text: string; var Index: Integer): Boolean;
function ScanString(const S: string; const Text: string; var Index: Integer): Boolean;
function ScanCompare(const S: string; const Text: string; Index: Integer): Boolean;

function StringRepeat(const S: string; C: Integer): string;
function FormatColLine(Column, Line: Integer): string;
//If index can less than str length, usefull to port it to another language like c,d
function IndexInStr(Index: Integer; const Str: string): Boolean; inline;
//AToIndex not included
function SliceText(const AText: String; const AFromIndex, AToIndex: Integer): String;

implementation

uses
  StrUtils;

function FormatColLine(Column, Line: Integer): string;
begin
   Result := '[Line: ' + IntToStr(Line) + ', ' + IntToStr(Column) + ']';
end;

function IndexInStr(Index: Integer; const Str: string): Boolean;
begin
  Result := Index <= Length(Str);
  //Result := Index < Length(Str); // in C,D
end;

function SliceText(const AText: String; const AFromIndex, AToIndex: Integer): String;
begin
  Result := Copy(AText, AFromIndex, AToIndex - AFromIndex);
end;

procedure RaiseError(AError: string; Line: Integer; Column: Integer);
begin
  if Line > 0 then
  begin
    raise ESardParserException.Create(AError, Line, Column)
    {$ifdef FPC}
    at get_caller_addr(get_frame), get_caller_frame(get_frame)
    {$endif};
  end
  else
  begin
    raise EsardException.Create(AError)
    {$ifdef FPC}
    at get_caller_addr(get_frame), get_caller_frame(get_frame)
    {$endif};
  end;
end;

function ScanCompare(const S: string; const Text: string; Index: Integer): Boolean;
var
  i: Integer;
begin
  if S = '' then
    Result := False
  else
  begin
    Result := (Length(Text) - (Index - 1)) >= length(S);
    //Result := (Length(Text) - Index) >= length(S); //when convert to C, D
    if Result then
    begin
      for i := 1 to Length(S) do
      begin
        if Text[Index+ i - 1] <> s[i] then
        begin
          Result := False;
          break;
        end;
      end;
    end;
  end;
end;

function ScanText1(const S: string; const Text: string; var Index: Integer): Boolean;
begin
  if S = '' then
    Result := False
  else
  begin
    Result := (Length(Text) - (Index - 1)) >= length(S);
    //Result := (Length(Text) - Index) >= length(S); //when convert to C, D
    if Result then
    begin
      Result := LowerCase(MidStr(Text, Index, Length(S))) = LowerCase(S); //caseinsensitive  :-S need to improve @BILAL help me please
      if Result then
        Index := Index + Length(S);
    end;
  end;
end;

function ScanString(const S: string; const Text: string; var Index: Integer): Boolean;
begin
  Result := ScanCompare(S, Text, Index);
  if Result then
    Index := Index + Length(S);
end;

function StringRepeat(const S: string; C: Integer): string;
begin
  Result := '';
  while C > 0 do
  begin
    Result := Result + S;
    C := C-1;
  end;
end;

{ TSardNamedObjects }

function TSardNamedObjects<_Object_>.Scan(const vText: string; vIndex: Integer): _Object_;
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
      if max < Length(Items[i].Name) then
      begin
        max := Length(Items[i].Name);
        Result := Items[i];
      end;
    end;
  end;
end;

function TSardNamedObjects<_Object_>.IsOpenBy(C: Char): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    if Items[i].IsOpenBy(C) then
    begin
      Result := True;
      exit;
    end;
  end;
  Result := False;
end;

procedure TSardNamedObject.ExportWrite(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin

end;

{ EsardParserException }

constructor ESardParserException.Create(const Msg: string; const Line, Column: Integer);
begin
  inherited Create(Msg + ' at ' + FormatColLine(Column, Line));
  FLine := Line;
  FColumn := Column;
end;

{ TSardStack }

function TSardStack<_Node_>.Pull: _Node_;
var
  aItem: TSardStackItem;
begin
  if FTop = nil then
    RaiseError('Stack is empty');
  BeforePop;
  Result := FTop.Node;
  aItem := FTop;
  FTop := aItem.Parent;
  aItem.Free;
  Dec(FCount);
  BeforePop;
end;

procedure TSardStack<_Node_>.SetCurrent(vNode: _Node_; OwnIt: Boolean);
begin
  if Top = nil then
    RaiseError('Can'' set to current is nil');
  Top.SetObject(vNode);
end;

procedure TSardStack<_Node_>.Pop;
var
  aNode: _Node_;
begin
  {$ifdef VERBOSE}
  Write('POP: ' + Current.ClassName);
  {$endif}
  aNode := Pull;
  if Own then
    aNode.Free;
  {$ifdef VERBOSE}
  if Current <> nil then
    WriteLn(' TO: ' + Current.ClassName)
  else
    WriteLn;
  {$endif}
end;

function TSardStack<_Node_>.GetParent: _Node_;
begin
  if FTop = nil then
    Result := nil
  else if FTop.Parent = nil then
    Result := nil
  else
    Result := FTop.Parent.Node;
end;

function TSardStack<_Node_>.GetCurrent: _Node_;
begin
  if FTop = nil then
    Result := nil
  else
    Result := FTop.Node;
end;

procedure TSardStack<_Node_>.AfterPush;
begin
end;

procedure TSardStack<_Node_>.BeforePop;
begin
end;

constructor TSardStack<_Node_>.Create(OwnItems: Boolean);
begin
  inherited Create;
  Own := OwnItems;
end;

destructor TSardStack<_Node_>.Destroy;
begin
  if Own then
    Clear;
  inherited Destroy;
end;

procedure TSardStack<_Node_>.Clear;
begin
  while Top <> nil do
    Pop;
end;

function TSardStack<_Node_>.IsEmpty: Boolean;
begin
  Result := FTop = nil;
end;

function TSardStack<_Node_>.Peek: _Node_;
begin
  if FTop = nil then //TODO maybe return nil<--
    RaiseError('Stack is empty');
  Result := FTop.Node;
end;

procedure TSardStack<_Node_>.Push(vObject: _Node_; OwnIt: Boolean = True);
var
  aItem: TSardStackItem;
begin
  if vObject = nil then
    RaiseError('Can'' push nil');
  aItem := TSardStackItem.Create;
  aItem.Parent := FTop;
  aItem.Owner := Self;
  if FTop = nil then
    aItem.Level := 0
  else
    aItem.Level := FTop.Level + 1;
  FTop := aItem;
  Inc(FCount);
  SetCurrent(vObject, OwnIt);
  if vObject is TSardStackObject then
    TSardStackObject(vObject).Level := FCount;
  AfterPush;
  {$ifdef VERBOSE}
  WriteLn('PUSH: ' + Current.ClassName);
  {$endif}
end;

{ TSourceWriter }

constructor TSourceWriter.Create;
begin
  inherited Create;
  TabWidth := 4;
end;

procedure TSourceWriter.Add(Level: Integer; S: string);
begin
  Add(StringOfChar(' ', Level * TabWidth) + S);
end;

{ TStringSourceWriter }

constructor TStringSourceWriter.Create(Strings: TStrings);
begin
  inherited Create;
  FStrings := Strings;
end;

destructor TStringSourceWriter.Destroy;
begin
  Flush;
  inherited;
end;

procedure TStringSourceWriter.Flush;
begin
  if FLine <> '' then
    NewLine;
end;

procedure TStringSourceWriter.Add(const S: string);
begin
  FLine := FLine + S;
end;

procedure TStringSourceWriter.NewLine;
begin
  FStrings.Add(FLine);
  FLine := '';
end;

{ TSardString }

constructor TSardString.Create(const AValue: String; AStartPos: Integer = 1; AEndPos: Integer = -1);
begin
  FValue := AValue;
  FStartPos := AStartPos;
  FEndPos := AEndPos;
end;

function TSardNamedObject.IsOpenBy(C: Char): Boolean;
begin
  Result := (Name <> '') and (Name[1] = C);
end;

{ TSardStack<_Node_>.TSardStackItem }

procedure TSardStack<_Node_>.TSardStackItem.SetObject(vNode: _Node_; OwnIt: Boolean);
var
  aNode: _Node_;
begin
  aNode := Node;
  if OwnIt then
    FreeAndNil(aNode);
  Node := vNode;
  OwnIt := OwnIt;
end;

end.
