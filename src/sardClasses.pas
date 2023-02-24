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

  { TsardStack }

  TSardStack<_Object_: class> = class(TSardObject)
  protected
    Own: Boolean;
    type

      TSardStackItem = class(TSardObject)
      protected
        AnObject: _Object_;
        Parent: TSardStackItem;
      public
        Owner: TSardStack<_Object_>;
        Level: Integer;
      end;

  private
    FCount: Integer;
    FCurrentItem: TSardStackItem;
  protected
    function GetParent: _Object_;
    function GetCurrent: _Object_;
    procedure AfterPush; virtual;
    procedure BeforePop; virtual;
  public
    constructor Create(OwnItems: Boolean); virtual;
    destructor Destroy; override;
    procedure Clear;
    function IsEmpty: Boolean;
    procedure Push(vObject: _Object_); overload;
    function Pull: _Object_; //Pop but do not delete the object
    procedure Pop;
    function Peek: _Object_;
    property Current: _Object_ read GetCurrent;
    property Parent: _Object_ read GetParent;
    property CurrentItem: TSardStackItem read FCurrentItem;
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
begin
  Result := ScanString(S, Text, Index);
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
      if Result then
        Index := Index + Length(S);
    end;
  end;
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
  item: TSardNamedObject;
begin
  for item in Self do
  begin
    if item.IsOpenBy(C) then
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

function TSardStack<_Object_>.Pull: _Object_;
var
  aItem: TSardStackItem;
begin
  if FCurrentItem = nil then
    RaiseError('Stack is empty');
  BeforePop;
  Result := FCurrentItem.AnObject;
  aItem := FCurrentItem;
  FCurrentItem := aItem.Parent;
  aItem.Free;
  Dec(FCount);
  BeforePop;
end;

procedure TSardStack<_Object_>.Pop;
var
  aObject: _Object_;
begin
  {$ifdef VERBOSE}
  Write('POP: ' + Current.ClassName);
  {$endif}
  aObject := Pull;
  if Own then
    aObject.Free;
  {$ifdef VERBOSE}
  if Current <> nil then
    WriteLn(' TO: ' + Current.ClassName)
  else
    WriteLn;
  {$endif}
end;

function TSardStack<_Object_>.GetParent: _Object_;
begin
  if FCurrentItem = nil then
    Result := nil
  else if FCurrentItem.Parent = nil then
    Result := nil
  else
    Result := FCurrentItem.Parent.AnObject;
end;

function TSardStack<_Object_>.GetCurrent: _Object_;
begin
  if FCurrentItem = nil then
    Result := nil
  else
    Result := FCurrentItem.AnObject;
end;

procedure TSardStack<_Object_>.AfterPush;
begin
end;

procedure TSardStack<_Object_>.BeforePop;
begin
end;

constructor TSardStack<_Object_>.Create(OwnItems: Boolean);
begin
  inherited Create;
  Own := OwnItems;
end;

destructor TSardStack<_Object_>.Destroy;
begin
  if Own then
    Clear;
  inherited Destroy;
end;

procedure TSardStack<_Object_>.Clear;
begin
  while CurrentItem <> nil do
    Pop;
end;

function TSardStack<_Object_>.IsEmpty: Boolean;
begin
  Result := FCurrentItem = nil;
end;

function TSardStack<_Object_>.Peek: _Object_;
begin
  if FCurrentItem = nil then //TODO maybe return nil<--
    RaiseError('Stack is empty');
  Result := FCurrentItem.AnObject;
end;

procedure TSardStack<_Object_>.Push(vObject: _Object_);
var
  aItem: TSardStackItem;
begin
  if vObject = nil then
    RaiseError('Can''_Object_ push nil');
  aItem := TSardStackItem.Create;
  aItem.AnObject := vObject;
  aItem.Parent := FCurrentItem;
  aItem.Owner := Self;
  if FCurrentItem = nil then
    aItem.Level := 0
  else
    aItem.Level := FCurrentItem.Level + 1;
  FCurrentItem := aItem;
  Inc(FCount);
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

end.
