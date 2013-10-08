unit sardClasses;
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

{TODO:
  Check S is not empty before push
  Push with the scanner class and id

  Result := Integer + Float <-- it convert to float or not, hmmm not sure
}
{
  I use prime prefix to classes mean: Custom or Abstract that classes inherited from it
}

interface

uses
  Classes, SysUtils, Contnrs;

type
  Float = type extended;
  Int = type Int64;

type
  EsardException = class(Exception)
  private
    FCode: Cardinal;
  public
    property Code: Cardinal read FCode write FCode;
  end;

  EsardParserException = class(Exception)
  private
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(const Msg: string; const Column, Line: Integer);
    property Column: Integer read FColumn write FColumn;
    property Line: Integer read FLine write FLine;
  end;

  //Base classes

  { TsardObject }

  TsardObject = class(TObject)
  protected
    procedure Created; virtual;
  public
    procedure AfterConstruction; override;
  end;

  { TsardObjectList }

  TsardObjectList = class(TObjectList)
  protected
    procedure Created; virtual;
  public
    procedure AfterConstruction; override;
  end;

  TsardControl = (
    ctlStart, //Start parsing
    ctlStop, //Start parsing
    ctlDeclare, //Declare a class of object
    ctlAssign, //Assign to object/variable
    ctlNext, //End Params, Comma
    ctlEnd, //End Statement Semicolon
    ctlOpenBlock, // {
    ctlCloseBlock, // }
    ctlOpenParams, // (
    ctlCloseParams, // )
    ctlOpenArray, // [
    ctlCloseArray, // ]
    ctlNone
  );

  TsardTokinKind = (tkComment, tkIdentifier, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TsardLexical = class;
  TsardFeeder = class;
  TsardParser = class;
  TsardStack = class;

  TsardScannerClass = class of TsardScanner;

  { TsardScanner }

  TsardScanner = class(TsardObject)
  private
    FLexical: TsardLexical;
  protected
    //Return true if it done, next will auto detect it detect
    function Scan(const Text: string; var Column: Integer): Boolean; virtual; abstract;
    function Accept(const Text: string; var Column: Integer): Boolean; virtual;
    //This function call when switched to it
    procedure Switched; virtual;
  public
    Collected: string; //buffer
    Scanner: TsardScanner;
    constructor Create(vLexical: TsardLexical); virtual;
    destructor Destroy; override;
    property Lexical: TsardLexical read FLexical;
  end;

  { TsardLexical }

  TsardLexical = class(TsardObjectList)
  private
    FLine: Integer;
    FParser: TsardParser;
    FScanner: TsardScanner;
    function GetItem(Index: Integer): TsardScanner;
    procedure SetParser(AValue: TsardParser);
  public
    constructor Create(vParser: TsardParser);
    {
      Open mean first char in it, like Numbers must start with number 0..9 but can contain a..z
        or Identifier start a..z or _ but can contain numbers
    }
    function IsWhiteSpace(vChar: AnsiChar; vOpen: Boolean = True): Boolean; virtual; abstract;
    function IsControl(vChar: AnsiChar): Boolean; virtual; abstract;
    function IsOperator(vChar: AnsiChar): Boolean; virtual; abstract;
    function IsNumber(vChar: AnsiChar; vOpen: Boolean = True): Boolean; virtual; abstract;
    function IsIdentifier(vChar: AnsiChar; vOpen: Boolean = True): Boolean; virtual;

    function DetectScanner(const Text: string; var Column: Integer): TsardScanner;
    procedure SwitchScanner(NextScanner: TsardScanner);
    //This find the class and switch to it
    procedure SelectScanner(ScannerClass: TsardScannerClass);
    function Find(const ScannerClass: TsardScannerClass): TsardScanner;
    procedure ScanLine(const Text: string; const ALine: Integer);
    function AddScanner(ScannerClass: TsardScannerClass): TsardScanner;
    property Items[Index: Integer]: TsardScanner read GetItem; default;
    property Scanner: TsardScanner read FScanner;
    property Parser: TsardParser read FParser ;//write SetParser;
    property Line: Integer read FLine;
  end;

  { TsardFeeder }

  TsardFeeder = class(TsardObject)
  private
    FActive: Boolean;
    FVersion: string;
    FCharset: string;
    FLexical: TsardLexical;//TODO use stacker
    procedure SetLexical(AValue: TsardLexical);
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
  public
    constructor Create(vLexical: TsardLexical);
    destructor Destroy; override;
    procedure ScanLine(const Text: string; const Line: Integer);
    procedure Scan(const Lines: TStrings);
    //procedure Scan(const FileName: string); overload; //TODO
    //procedure Scan(const Stream: TStream); overload; //TODO
    //procedure Scan(const Stream: IStream); overload; //TODO

    procedure Start;
    procedure Stop;
    property Active: Boolean read FActive write FActive;
    property Version: string read FVersion write FVersion;
    property Charset: string read FCharset write FCharset;
    property Lexical: TsardLexical read FLexical write SetLexical;

  end;

  TsardStackItem = class(TsardObject)
  protected
    AnObject: TObject;
    Parent: TsardStackItem;
  public
    Owner: TsardStack;
    Level: Integer;
  end;

  { TsardStack }

  TsardStack = class(TsardObject)
  private
    FCount: Integer;
    FCurrentItem: TsardStackItem;
  protected
    function GetParent: TObject;
    function GetCurrent: TObject;
    procedure AfterPush; virtual;
    procedure BeforePop; virtual;
  public
    function IsEmpty: Boolean;
    procedure Push(vObject: TObject);
    procedure Pop;
    function Pull: TObject; //Pop but do not delete delete the ibject
    function Peek: TObject;
    property Current: TObject read GetCurrent;
    property Parent: TObject read GetParent;
    property CurrentItem: TsardStackItem read FCurrentItem;
    property Count: Integer read FCount;
  end;

  TsrdType = (tpNone, tpIdentifier, tpNumber, tpColor, tpString, tpComment);

  { TsardParser }

  TsardParser = class(TsardStack)
  protected
    procedure Start; virtual;
    procedure Stop; virtual;
  public
    procedure TriggerToken(AToken: String; AType: TsrdType); virtual; abstract;
    procedure TriggerOperator(AOperator: TsardObject); virtual; abstract;
    procedure TriggerControl(AControl: TsardControl); virtual; abstract;
  end;

  { TsardPrimeEngine }

  TsardPrimeEngine = class(TsardObject)
  private
  protected
    procedure Created; override;
  public
  end;

procedure RaiseError(AError: string);
function ScanCompare(S: string; const Text: string; const Index: Integer): Boolean;
function ScanText(S: string; const Text: string; var Index: Integer): Boolean;
function StringRepeat(S: string; C: Integer): string;

implementation

uses
  StrUtils;

procedure RaiseError(AError: string);
begin
  raise EsardException.Create(AError) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;

function ScanCompare(S: string; const Text: string; const Index: Integer): Boolean;
begin
  Result := (Length(Text) - Index) >= length(S);
  if Result then
    Result := LowerCase(MidStr(Text, Index, Length(S))) = LowerCase(S); //caseinsensitive
end;

function ScanText(S: string; const Text: string; var Index: Integer): Boolean;
begin
  Result := (Length(Text) - Index) >= length(S);
  if Result then
    Result := LowerCase(MidStr(Text, Index, Length(S))) = LowerCase(S); //caseinsensitive
  if Result then
    Index := Index + Length(S);
end;

function StringRepeat(S: string; C: Integer): string;
begin
  Result := '';
  while C > 0 do
  begin
    Result := Result + S;
    C := C-1;
  end;
end;

{ TsardParser }

procedure TsardParser.Start;
begin
end;

procedure TsardParser.Stop;
begin
end;


{ TsardObjectList }

procedure TsardObjectList.Created;
begin
end;

procedure TsardObjectList.AfterConstruction;
begin
  inherited AfterConstruction;
  Created;
end;

{ TsardObject }

procedure TsardObject.Created;
begin
end;

procedure TsardObject.AfterConstruction;
begin
  inherited AfterConstruction;
  Created;
end;

{ TsardPrimeEngine }

procedure TsardPrimeEngine.Created;
begin
  inherited Created;
end;

function TsardLexical.IsIdentifier(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  Result := not IsWhiteSpace(vChar) and not IsControl(vChar) and not IsOperator(vChar);
  if vOpen then
    Result := Result and not IsNumber(vChar, vOpen);
end;

{ TsardParser }

procedure TsardStack.Pop;
var
  aItem: TsardStackItem;
  aObject: TObject;
begin
  if FCurrentItem = nil then
    raise EsardException.Create('Stack is empty');
  aObject := FCurrentItem.AnObject;
  aItem := FCurrentItem;
  FCurrentItem := aItem.Parent;
  Dec(FCount);
  aItem.Free;
  aObject.Free;
  BeforePop;
end;

function TsardStack.GetParent: TObject;
begin
  if FCurrentItem = nil then
    Result := nil
  else if FCurrentItem.Parent = nil then
    Result := nil
  else
    Result := FCurrentItem.Parent.AnObject;
end;

function TsardStack.GetCurrent: TObject;
begin
{  if FCurrentItem = nil then
    RaiseError('Stack is empty');}
  if FCurrentItem = nil then
    Result := nil
  else
    Result := FCurrentItem.AnObject;
end;

procedure TsardStack.AfterPush;
begin
end;

procedure TsardStack.BeforePop;
begin
end;

function TsardStack.IsEmpty: Boolean;
begin
  Result := FCurrentItem = nil;
end;

function TsardStack.Peek: TObject;
begin
  if FCurrentItem = nil then
    RaiseError('Stack is empty');
  Result := FCurrentItem.AnObject;
end;

function TsardStack.Pull: TObject;
var
  aItem: TsardStackItem;
begin
  if FCurrentItem = nil then
    RaiseError('Stack is empty');
  Result := FCurrentItem.AnObject;
  aItem := FCurrentItem;
  FCurrentItem := aItem.Parent;
  aItem.Free;
  Dec(FCount);
  BeforePop;
end;

procedure TsardStack.Push(vObject: TObject);
var
  aItem: TsardStackItem;
begin
  if vObject = nil then
    RaiseError('Can''t push nil');
  aItem := TsardStackItem.Create;
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
end;

function FormatColLine(Column, Line: Integer): string;
begin
   Result := 'Line Number ' + IntToStr(Line) + ', Column ' + IntToStr(Column);
end;

constructor EsardParserException.Create(const Msg: string; const Column, Line: Integer);
begin
  inherited Create(Msg + #13 +FormatColLine(Column, Line));
  FLine := Line;
  FColumn := Column;
end;

{ TsardScanner }

function TsardScanner.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := False;
end;

procedure TsardScanner.Switched;
begin
  //Maybe reset buffer or something
end;

constructor TsardScanner.Create(vLexical: TsardLexical);
begin
  inherited Create;
  FLexical := vLexical;
end;

destructor TsardScanner.Destroy;
begin
  inherited Destroy;
end;

{ TsardLexical }

function TsardLexical.GetItem(Index: Integer): TsardScanner;
begin
  Result := inherited Items[Index] as TsardScanner;
end;

procedure TsardLexical.SetParser(AValue: TsardParser);
begin
  if FParser = AValue then
    Exit;
  FParser := AValue;
end;

constructor TsardLexical.Create(vParser: TsardParser);
begin
  inherited Create;
  FParser := vParser;
end;

function TsardLexical.DetectScanner(const Text: string; var Column: Integer): TsardScanner;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if (Items[i] <> Result) and Items[i].Accept(Text, Column) then
    begin
      Result := Items[i];
      break;
    end;
  end;
  if Result = nil then
    RaiseError('Scanner not found:' + Text[Column]);
  SwitchScanner(Result);
end;

function TsardLexical.Find(const ScannerClass: TsardScannerClass): TsardScanner;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if ScannerClass = Items[i].ClassType then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TsardLexical.AddScanner(ScannerClass: TsardScannerClass): TsardScanner;
begin
  Result := ScannerClass.Create(Self);
  inherited Add(Result);
end;

procedure TsardFeeder.Stop;
begin
  if not FActive then
    RaiseError('File already closed');
  Lexical.Parser.Stop;
  DoStop;
  FActive := False;
end;


procedure TsardFeeder.Start;
begin
  if FActive then
    RaiseError('File already opened');
  FActive := True;
  DoStart;
  Lexical.Parser.Start;
end;

procedure TsardFeeder.SetLexical(AValue: TsardLexical);
begin
  if FLexical =AValue then Exit;
  if Active then
    RaiseError('You can not set scanner when started!');
  FLexical :=AValue;
end;

procedure TsardFeeder.DoStart;
begin
end;

procedure TsardFeeder.DoStop;
begin
end;

procedure TsardLexical.SwitchScanner(NextScanner: TsardScanner);
begin
  if FScanner <> NextScanner then
  begin
    FScanner := NextScanner;
    if FScanner <> nil then
      FScanner.Switched;
  end;
end;

procedure TsardLexical.SelectScanner(ScannerClass: TsardScannerClass);
var
  aScanner: TsardScanner;
begin
  aScanner := Find(ScannerClass);
  if aScanner = nil then
    RaiseError('Scanner not found');
  SwitchScanner(aScanner);
end;

constructor TsardFeeder.Create(vLexical: TsardLexical);
begin
  inherited Create;
  FVersion := '1.0';
  {$ifdef FPC}
  FCharset := 'utf-8';
  {$else}
  FCharset := 'iso-8859-1';
  {$endif}

  FLexical := vLexical;
end;

destructor TsardFeeder.Destroy;
begin
  inherited Destroy;
end;

procedure TsardFeeder.ScanLine(const Text: string; const Line: Integer);
begin
  if not Active then
    RaiseError('Feeder not started');
  Lexical.ScanLine(Text, Line);
end;

procedure TsardLexical.ScanLine(const Text: string; const ALine: Integer);
var
  Column, OldColumn: Integer;
  OldScanner: TsardScanner;
  l: Integer;
  Done: Boolean;
begin
  FLine := ALine;
  Column := 1; //start of pascal string is 1
  l := Length(Text);
  if Scanner = nil then
    DetectScanner(Text, Column);
  while (Column <= l) do
  begin
    OldColumn := Column;
    OldScanner := FScanner;
    try
      if Scanner.Scan(Text, Column) then
        DetectScanner(Text, Column);

      if (OldColumn = Column) and (OldScanner = FScanner) then
        RaiseError('Feeder in loop with: ' + FScanner.ClassName);
    except
      on E: EsardException do
      begin
        raise EsardParserException.Create(E.Message, Column, Line);
      end;
    end;
  end;
end;

procedure TsardFeeder.Scan(const Lines: TStrings);
var
  i: Integer;
begin
  Start;
  for i := 0 to Lines.Count -1 do
  begin
    ScanLine(Lines[i] + #13, i + 1);
  end;
  Stop;
end;

end.
