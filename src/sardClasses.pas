unit sardClasses;
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

{TODO:
  Check S is not empty before push
  Push with the scanner class and id

  Result := Integer + Float <-- it convert to float or not, hmmm not sure
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
    FLine: Integer;
    FColumn: Integer;
    FCode: Cardinal;
  public
    constructor Create(const Msg: string); overload;
    constructor Create(const Msg: string; const Column, Line: Integer); overload;
    property Code: Cardinal read FCode write FCode;
    property Column: Integer read FColumn write FColumn;
    property Line: Integer read FLine write FLine;
  end;

  EsardParserException = class(EsardException);

  //Base classes

  TsardObject = class(TObject);
  TsardObjectList = class(TObjectList);

  TsardControl = (ctlDeclare, ctlAssign, ctlOpenBracket, ctlCloseBracket, ctlOpenSquare, ctlCloseSquare, ctlOpenBlock, ctlCloseBlock, ctlPoInter, ctlSplit, ctlFinish, ctlComma, ctlSemicolon);
  TsardBracketKind = (brBracket, brSquare, brCurly);// and (), [], {} or maybe <>
  TsardTokinKind = (tkComment, tkIdentifier, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TsardScannerID = type Integer;

  TsardScanners = class;
  TsardFeeder = class;
  TsardParser = class;

  TsardScannerClass = class of TsardScanner;

  { TsardScanner }

  TsardScanner = class(TsardObject)
  private
    FScanners: TsardScanners;
  protected
    function CheckText(S: string; const Text: string; const Column: Integer): Boolean;
    function ScanText(S: string; const Text: string; var Column: Integer): Boolean;
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer); virtual; abstract;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; virtual;
    function DetectScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
    procedure SelectScanner(AScannerClass: TsardScannerClass);
  public
    Index: TsardScannerID;
    Collected: string; //buffer
    Scanner: TsardScannerID;
    constructor Create(vScanners: TsardScanners); virtual;
    destructor Destroy; override;
    property Scanners: TsardScanners read FScanners;
  end;

  { TsardScanners }

  TsardScanners = class(TsardObjectList)
  private
    FParser: TsardParser;
    FScannerID: TsardScannerID;
    FFeeder: TsardFeeder;
    function GetItem(Index: Integer): TsardScanner;
    procedure SetParser(AValue: TsardParser);
  public
    constructor Create(vFeeder: TsardFeeder; FreeObjects: boolean = True); virtual;
    function DetectScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
    procedure SwitchScanner(NextScanner: TsardScannerID);
    procedure SelectScanner(ScannerClass: TsardScannerClass);
    function Find(const ScannerClass: TsardScannerClass): TsardScanner;
    procedure ScanLine(const Text: string; const Line: Integer);
    function RegisterScanner(ScannerClass: TsardScannerClass): TsardScannerID;
    property Items[Index: Integer]: TsardScanner read GetItem; default;
    property ScannerID: TsardScannerID read FScannerID;
    property Feeder: TsardFeeder read FFeeder;
    property Parser: TsardParser read FParser write SetParser;
  end;

  { TsardFeeder }

  TsardFeeder = class(TsardObject)
  private
    FActive: Boolean;
    FVersion: string;
    FCharset: string;
    FScanners: TsardScanners;//TODO use stacker
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;

  public
    constructor Create; virtual;
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
    property Scanners: TsardScanners read FScanners;
  end;

  { TsardParser }

  TsardParser = class(TsardObject)
  protected
  public
    procedure TriggerOpen(vBracket: TsardBracketKind); virtual; abstract;
    procedure TriggerClose(vBracket: TsardBracketKind); virtual; abstract;
    procedure TriggerToken(Token: String; TokenID: Integer); virtual; abstract;
    procedure TriggerOperator(AOperator: TsardObject); virtual; abstract; //TsardoOperator
    procedure TriggerControl(AControl: TsardControl); virtual;
  end;

  TsardStack = class;

  TsardStackItem = class(TsardObject)
  public
    AnObject: TsardObject;
    Owner: TsardStack;
    Prior: TsardStackItem;
  end;

  { TsardStack }

  TsardStack = class(TsardObject)
  private
    FCount: Integer;
    FCurrent: TsardStackItem;
    function GetCurrent: TsardObject;
    procedure SetCurrent(const AValue: TsardObject);
  public
    function IsEmpty: Boolean;
    procedure Push(vObject: TsardObject);
    function Pop: TsardObject;
    procedure Delete;
    function Peek: TsardObject;
    property Current: TsardObject read GetCurrent write SetCurrent;
    property Count: Integer read FCount;
  end;

  { TsardCustomEngine }

  TsardCustomEngine = class(TsardObject)
  private
  protected
    procedure Created; virtual;
  public
    procedure AfterConstruction; override;
    {
      Open mean first char in it, like Numbers must start with number 0..9 but can contain a..z
        or Identifier start a..z or _ but can contain numbers
    }
    function IsWhiteSpace(vChar: AnsiChar; vOpen: Boolean = True): Boolean; virtual; abstract;
    function IsControl(vChar: AnsiChar; vOpen: Boolean = True): Boolean; virtual; abstract;
    function IsOperator(vChar: AnsiChar; vOpen: Boolean = True): Boolean; virtual; abstract;
    function IsNumber(vChar: AnsiChar; vOpen: Boolean = True): Boolean; virtual; abstract;
    function IsIdentifier(vChar: AnsiChar; vOpen: Boolean = True): Boolean; virtual;
  end;

implementation

uses
  StrUtils;

{ TsardCustomEngine }

procedure TsardCustomEngine.Created;
begin
end;

procedure TsardCustomEngine.AfterConstruction;
begin
  inherited AfterConstruction;
  Created;
end;

function TsardCustomEngine.IsIdentifier(vChar: AnsiChar; vOpen: Boolean): Boolean;
begin
  Result := not IsWhiteSpace(vChar) and not IsControl(vChar) and not IsOperator(vChar, vOpen);
  if vOpen then
    Result := Result and not IsNumber(vChar, vOpen);
end;

{ TsardParser }

procedure TsardParser.TriggerControl(AControl: TsardControl);
begin
end;

procedure TsardStack.Delete;
var
  aNode: TsardStackItem;
  aObject: TsardObject;
begin
  if FCurrent = nil then
    raise EsardException.Create('Stack is empty');
  aObject := FCurrent.AnObject;
  aNode := FCurrent;
  FCurrent := aNode.Prior;
  Dec(FCount);
  aNode.Free;
  aObject.Free;
end;

function TsardStack.GetCurrent: TsardObject;
begin
  if FCurrent = nil then
    raise EsardException.Create('Stack is empty');
  Result := FCurrent.AnObject;
end;

function TsardStack.IsEmpty: Boolean;
begin
  Result := FCurrent = nil;
end;

function TsardStack.Peek: TsardObject;
begin
  if FCurrent = nil then
    raise EsardException.Create('Stack is empty');
  Result := FCurrent.AnObject;
end;

function TsardStack.Pop: TsardObject;
var
  aNode: TsardStackItem;
begin
  if FCurrent = nil then
    raise EsardException.Create('Stack is empty');
  Result := FCurrent.AnObject;
  aNode := FCurrent;
  FCurrent := aNode.Prior;
  aNode.Free;
  Dec(FCount);
end;

procedure TsardStack.Push(vObject: TsardObject);
var
  aNode: TsardStackItem;
begin
  aNode := TsardStackItem.Create;
  aNode.AnObject := vObject;
  aNode.Prior := FCurrent;
  aNode.Owner := Self;
  FCurrent := aNode;
  Inc(FCount);
end;

procedure TsardStack.SetCurrent(const AValue: TsardObject);
begin
  if FCurrent = nil then
    raise EsardException.Create('Stack is empty');
  FCurrent.AnObject := AValue;
end;

{ EmnXMLException }

constructor EsardException.Create(const Msg: string; const Column, Line: Integer);
begin
  Create(Msg + #13'Line Number ' + IntToStr(Line) + ', Column ' + IntToStr(Column));
  FLine := Line;
  FColumn := Column;
end;

constructor EsardException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

{ TsardScanner }

function TsardScanner.CheckText(S: string; const Text: string; const Column: Integer): Boolean;
begin
  Result := (Length(Text) - Column) >= length(S);
  if Result then
    Result := LowerCase(MidStr(Text, Column, Length(S))) = LowerCase(S); //caseinsensitive
end;

function TsardScanner.ScanText(S: string; const Text: string; var Column: Integer): Boolean;
begin
  Result := (Length(Text) - Column) >= length(S);
  if Result then
    Result := LowerCase(MidStr(Text, Column, Length(S))) = LowerCase(S); //caseinsensitive
  if Result then
    Column := Column + Length(S);
end;
(*
procedure TsardScanner.ScanTo(NextScanner: TsardScannerID; const SubStr, Text: string; var Column: Integer; const Line: Integer);
var
  p: Integer;
  l, c, i: Integer;
begin
  p := 0;
  c := 1;
  l := Length(SubStr);
  for i := Column to Length(Text) do
  begin
    if not (Text[i] in sWhitespace) then
    begin
      if Text[i] = SubStr[c] then
      begin
        if c = l then
        begin
          p := i + 1;
          break;
        end;
        Inc(c);
      end
      else
        raise EsardParserException.Create('syntax error', Line, Column);
    end;
  end;

  if p > 0 then
  begin
    Column := p;
    SwitchScanner(NextScanner);
  end
  else
  begin
    Column := Length(Text) + 1;
    SwitchScanner(Scanner);
  end;
end;
*)
function TsardScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False;
end;

function TsardScanner.DetectScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
begin
  Result := Scanners.DetectScanner(Text, Column, Line);
end;

procedure TsardScanner.SelectScanner(AScannerClass: TsardScannerClass);
begin
  Scanners.SelectScanner(AScannerClass);
end;

constructor TsardScanner.Create(vScanners: TsardScanners);
begin
  inherited Create;
  FScanners := vScanners;
end;

destructor TsardScanner.Destroy;
begin
  inherited Destroy;
end;

{ TsardScanners }

function TsardScanners.GetItem(Index: Integer): TsardScanner;
begin
  Result := inherited Items[Index] as TsardScanner;
end;

procedure TsardScanners.SetParser(AValue: TsardParser);
begin
  if FParser = AValue then Exit;
  if Feeder.Active then
    raise EsardException.Create('You can'' set a parser while it is active!');
  FParser := AValue;
end;

constructor TsardScanners.Create(vFeeder: TsardFeeder; FreeObjects: boolean);
begin
  FFeeder := vFeeder;
  inherited Create(FreeObjects);
end;

function TsardScanners.DetectScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].Index <> Result) and Items[i].Accept(Text, Column, Line) then
    begin
      Result := i;
//      WriteLn('Accept: '+Items[i].ClassName);
      break;
    end;
  end;
  if Result < 0 then
    raise EsardException.Create('Scanner not found:' + Text[Column]);
  SwitchScanner(Result);
end;

function TsardScanners.Find(const ScannerClass: TsardScannerClass): TsardScanner;
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

function TsardScanners.RegisterScanner(ScannerClass: TsardScannerClass): TsardScannerID;
var
  aScanner: TsardScanner;
begin
  aScanner := ScannerClass.Create(Self);
  Result := Add(aScanner);
  aScanner.Index := Result;
end;

procedure TsardFeeder.Stop;
begin
  if not FActive then
    raise EsardException.Create('File already closed');
  DoStop;
  FActive := False;
end;


procedure TsardFeeder.Start;
begin
  if FActive then
    raise EsardException.Create('File already opened');
  FActive := True;
  DoStart;
end;

procedure TsardFeeder.DoStart;
begin
end;

procedure TsardFeeder.DoStop;
begin
end;

procedure TsardScanners.SwitchScanner(NextScanner: TsardScannerID);
begin
  if FScannerID <> NextScanner then
  begin
    FScannerID := NextScanner;
  end;
end;

procedure TsardScanners.SelectScanner(ScannerClass: TsardScannerClass);
var
  aScanner: TsardScanner;
begin
  aScanner := Find(ScannerClass);
  if aScanner = nil then
    raise EsardException.Create('Scanner not found');
  SwitchScanner(aScanner.Index);
end;

constructor TsardFeeder.Create;
begin
  inherited Create;
  FVersion := '1.0';
  {$ifdef FPC}
  FCharset := 'utf-8';
  {$else}
  FCharset := 'iso-8859-1';
  {$endif}
  FScanners := TsardScanners.Create(Self);
end;

destructor TsardFeeder.Destroy;
begin
  FreeAndNil(FScanners);
  inherited Destroy;
end;

procedure TsardFeeder.ScanLine(const Text: string; const Line: Integer);
begin
  if not Active then
    raise EsardException.Create('Feeder not started');
  Scanners.ScanLine(Text, Line);
end;

procedure TsardScanners.ScanLine(const Text: string; const Line: Integer);
var
  Column, OldColumn: Integer;
  OldScanner: TsardScannerID;
  l: Integer;
begin
  Column := 1; //start of pascal string is 1
  l := Length(Text);
  while (Column <= l) do
  begin
    OldColumn := Column;
    OldScanner := FScannerID;

    Items[ScannerID].Scan(Text, Column, Line);
    if Column <= Length(Text) then
      DetectScanner(Text, Column, Line);

    if (OldColumn = Column) and (OldScanner = FScannerID) then
      raise EsardException.Create('Feeder in loop with: ' + Items[FScannerID].ClassName);
  end;
end;

procedure TsardFeeder.Scan(const Lines: TStrings);
var
  i: Integer;
begin
  Start;
  for i := 0 to Lines.Count -1 do
  begin
    ScanLine(Lines[i] + #13, i);
  end;
  Stop;
end;

end.
