unit sard;
{**
 *  This file is part of the "SARD"
 *
 * @license   Apache License Version 2.0 (modified of http://www.gnu.org/licenses/lgpl.html)
 *            included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}

interface

uses
  Classes, SysUtils, contnrs,
  mnStreams;

const
   sSardAnsiOpen = '{?sard '; //with the space

type
  EsardException = class(Exception)
  private
    FLine: Integer;
    FColumn: Integer;
    FCode: Cardinal;
  public
    constructor Create(const Msg: string); overload;
    constructor Create(const Msg: string; Line: Integer; Column: Integer); overload;
    property Code: Cardinal read FCode write FCode;
    property Column: Integer read FColumn write FColumn;
    property Line: Integer read FLine write FLine;
  end;

  EsardParserException = class(EsardException);


  TsardFiler = class(TObject)
  private
    FActive: Boolean;
    FOwned: Boolean;
    FVersion: string;
    FStream: TmnWrapperStream;
    FHeader: TStringList;
    FCharset: string;
    FStandalone: Boolean;
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(Stream: TmnWrapperStream; Owned: Boolean = True); overload;
    constructor Create(const FileName:string); overload;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Active: Boolean read FActive write FActive;
    property Stream: TmnWrapperStream read FStream;
    property Header: TStringList read FHeader write FHeader;
    property Standalone: Boolean read FStandalone write FStandalone;
    property Version: string read FVersion write FVersion;
    property Charset: string read FCharset write FCharset;
  end;

  TsardScanType = (stNone, stHeader, stNormal, stString);
  TsardScanState = Integer; //(ssNone, ssHeader, ssNormal, ssSQString, ssDQString);

  TsardProcesses = class;
  TsardCustomScanner = class;

  { TsardProcess }

  TsardProcess = class(TObject)
  private
    FProcesses: TsardProcesses;
  public
    Index: Integer;
    Collected: string; //buffer
    State: TsardScanState;
    StateType: TsardScanType;
    constructor Create(vProcesses: TsardProcesses); virtual;
    procedure Close;
    procedure Scan(const Text: string; Line: Integer; var Column: Integer); virtual; abstract;
  end;

  TsardProcessClass = class of TsardProcess;

  { TsardProcesses }

  TsardProcesses = class(TObjectList)
  private
    FScanner: TsardCustomScanner;
    function GetItem(Index: Integer): TsardProcess;
  public
    constructor Create(vScanner: TsardCustomScanner; FreeObjects: boolean = True); virtual;
    function Find(const ProcessClass: TsardProcessClass): TsardProcess;
    function RegisterProcess(ProcessClass: TsardProcessClass): Integer;
    property Items[Index: Integer]: TsardProcess read GetItem;
    property Scanner: TsardCustomScanner read FScanner;
  end;

  { TsardScanner }

  TsardCustomScanner = class(TsardFiler)
  private
    FState: Integer;
    FCompleted: Boolean;
    FStarted: Boolean;
    FProcesses: TsardProcesses;
  protected
    procedure procNone(const Text: string; Line: Integer; var Column: Integer);
    procedure procHeader(const Text: string; Line: Integer; var Column: Integer);
    procedure procNormal(const Text: string; Line: Integer; var Column: Integer);
    procedure procDQString(const Text: string; Line: Integer; var Column: Integer);
    procedure procSQString(const Text: string; Line: Integer; var Column: Integer);
    procedure procSLComment(const Text: string; Line: Integer; var Column: Integer); //Single line comment
    procedure procComment(const Text: string; Line: Integer; var Column: Integer);

    procedure ChangeState(NextState: TsardScanState);

    procedure ScanBody(NextState: TsardScanState; const SubStr, Text: string; Line: Integer; var Column: Integer); deprecated;

    property Started:Boolean read FStarted;
    property Completed:Boolean read FCompleted;
    property Processes: TsardProcesses read FProcesses;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ParseLine(const Text: string; Line: Integer);
  end;

  TsardScanner = class(TsardCustomScanner)
  private
  protected
    prcNone: Integer;
    prcNormal: Integer;
    prcHeader: Integer;
    prcBlockComment: Integer;
    prcSLComment: Integer;
  public
    constructor Create; override;
  end;
{
  Common Processes
}

  { TsardNoneProcess }

  TsardNoneProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; Line: Integer; var Column: Integer);  override;
  end;

  { TsardNormalCommentProcess }

  TsardNormalProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; Line: Integer; var Column: Integer);  override;
  end;

  { TsardHeaderCommentProcess }

  TsardHeaderProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; Line: Integer; var Column: Integer);  override;
  end;

  { TsardSLCommentProcess }

  TsardSLCommentProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; Line: Integer; var Column: Integer);  override;
  end;

  { TsardBlockCommentProcess }

  TsardBlockCommentProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; Line: Integer; var Column: Integer);  override;
  end;

  { TsardSQStringProcess }

  TsardSQStringProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; Line: Integer; var Column: Integer);  override;
  end;

  { TsardDQStringProcess }

  TsardDQStringProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; Line: Integer; var Column: Integer);  override;
  end;

implementation

uses
  StrUtils;

{ TsardProcess }

constructor TsardProcess.Create(vProcesses: TsardProcesses);
begin
  inherited Create;
  FProcesses := vProcesses;
end;

procedure TsardProcess.Close;
begin

end;

{ TsardScanner }

constructor TsardScanner.Create;
begin
  inherited Create;

  with Processes do
  begin
    prcNone := RegisterProcess(TsardNoneProcess);
    prcNormal := RegisterProcess(TsardNormalProcess);
    prcHeader := RegisterProcess(TsardHeaderProcess);
    prcBlockComment := RegisterProcess(TsardBlockCommentProcess);
    prcSLComment := RegisterProcess(TsardSLCommentProcess);
  end;
end;

{ TsardDQStringProcess }

procedure TsardDQStringProcess.Scan(const Text: string; Line: Integer;
  var Column: Integer);
begin

end;

{ TsardSQStringProcess }

procedure TsardSQStringProcess.Scan(const Text: string; Line: Integer;
  var Column: Integer);
begin

end;

{ TsardBlockCommentProcess }

procedure TsardBlockCommentProcess.Scan(const Text: string; Line: Integer;
  var Column: Integer);
begin

end;

{ TsardSLCommentProcess }

procedure TsardSLCommentProcess.Scan(const Text: string; Line: Integer;
  var Column: Integer);
begin

end;

{ TsardHeaderCommentProcess }

procedure TsardHeaderProcess.Scan(const Text: string; Line: Integer;
  var Column: Integer);
begin

end;

{ TsardNormalCommentProcess }

procedure TsardNormalProcess.Scan(const Text: string; Line: Integer;
  var Column: Integer);
begin

end;

{ TsardNoneProcess }

procedure TsardNoneProcess.Scan(const Text: string; Line: Integer;
  var Column: Integer);
begin
  {if MidStr(Text, 1, Length(sSardAnsiOpen)) = sSardAnsiOpen then
  begin
    //There is a header and it is a Ansi document
    FStarted := True;
    FCompleted := False;
    Column := Column + Length(sSardAnsiOpen); //put the column to the first char of attributes of document
    ChangeState(ssHeader);
  end
  else
    ChangeState(ssNormal); //nop there is no header... skip to normal}
end;

{ TsardProcesses }

function TsardProcesses.GetItem(Index: Integer): TsardProcess;
begin
  Result := inherited Items[Index] as TsardProcess;
end;

constructor TsardProcesses.Create(vScanner: TsardCustomScanner; FreeObjects: boolean);
begin
  FScanner := vScanner;
  inherited Create(FreeObjects);
end;

function TsardProcesses.Find(const ProcessClass: TsardProcessClass): TsardProcess;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if ProcessClass = Items[i].ClassType then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TsardProcesses.RegisterProcess(ProcessClass: TsardProcessClass): Integer;
var
  aProcess: TsardProcess;
begin
  aProcess := ProcessClass.Create(Self);
  aProcess.Index := Add(aProcess);
end;

{ EmnXMLException }

constructor EsardException.Create(const Msg: string; Line,
  Column: Integer);
begin
  Create(Msg + #13'Line Number ' + IntToStr(Line) + ', Column ' + IntToStr(Column));
  FLine := Line;
  FColumn := Column;
end;

constructor EsardException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

procedure TsardFiler.Stop;
begin
  if not FActive then
    raise EsardException.Create('File already closed');
  DoStop;
  FActive := False;
end;

constructor TsardFiler.Create(Stream: TmnWrapperStream; Owned: Boolean);
begin
  Create;
  if Stream = nil then
    raise EsardException.Create('Stream is nil');
  FStream := Stream;
  FOwned := Owned;
end;

constructor TsardFiler.Create(const FileName: string);
begin
  Create(TmnWrapperStream.Create(TFileStream.Create(FileName, fmOpenRead)));
end;

constructor TsardFiler.Create;
begin
  inherited;
  FHeader := TStringList.Create;
  FVersion := '1.0';
  {$ifdef FPC}
  FCharset := 'utf-8';
  {$else}
  FCharset := 'iso-8859-1';
  {$endif}
end;

destructor TsardFiler.Destroy;
begin
{  if Active then
    Stop;}
  if FOwned then
    FStream.Free;
  FHeader.Free;
  inherited;
end;

procedure TsardFiler.DoStop;
begin
end;

procedure TsardFiler.DoStart;
begin
end;

procedure TsardFiler.Start;
begin
  if FActive then
    raise EsardException.Create('File already opened');
  FActive := True;
  DoStart;
end;

{ TsardCustomScanner }

procedure TsardCustomScanner.procNone(const Text: string; Line: Integer; var Column: Integer);
begin
end;

procedure TsardCustomScanner.procHeader(const Text: string; Line: Integer; var Column: Integer);
begin
  //ScanBody(ssNormal, '?}', Text, Line, Column)
end;

procedure TsardCustomScanner.procDQString(const Text: string; Line: Integer; var Column: Integer);
begin

end;

procedure TsardCustomScanner.procSQString(const Text: string; Line: Integer;
  var Column: Integer);
begin

end;

procedure TsardCustomScanner.procSLComment(const Text: string; Line: Integer;
  var Column: Integer);
begin

end;

procedure TsardCustomScanner.procComment(const Text: string; Line: Integer;
  var Column: Integer);
begin

end;

procedure TsardCustomScanner.procNormal(const Text: string; Line: Integer; var Column: Integer);
begin

end;

procedure TsardCustomScanner.ChangeState(NextState: TsardScanState);
begin
  if FState <> NextState then
  begin
    //FlushBuffer(FState);
    FState := NextState;
  end;
end;

procedure TsardCustomScanner.ScanBody(NextState: TsardScanState; const SubStr, Text: string; Line: Integer; var Column: Integer);
var
  p: integer;
begin
  p := PosEx(SubStr, Text, Column);
  if p > 0 then
  begin
    //AddBuffer(RangeStr(Text, Column, p - 1), NextState);
    Column := p + Length(SubStr);
  end
  else
  begin
    //AddBuffer(RangeStr(Text, Column, MaxInt), State);
    Column := Length(Text) + 1;
  end;
end;

constructor TsardCustomScanner.Create;
begin
  inherited Create;
  FProcesses := TsardProcesses.Create(Self);

end;

destructor TsardCustomScanner.Destroy;
begin
  FreeAndNil(FProcesses);
  inherited Destroy;
end;

procedure TsardCustomScanner.ParseLine(const Text: string; Line: Integer);
var
  Column, l: Integer;
begin
  if not Active then
    raise EsardException.Create('Scanner not started');
  Column := 1; //start of pascal string is 1
  l := Length(Text);
  while (Column <= l) do
  begin
    {if not Assigned(FParsers[FState]) then
      raise EsardException.Create('Parser state not assigned');}
    //FParsers[FState](Text, Line, Column);
  end;
end;

end.

