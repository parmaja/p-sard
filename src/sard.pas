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
  Classes, SysUtils, mnStreams;

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

  TsardScanState = (ssNone, ssHeader, ssProlog, ssText);

  TsardParserProc = procedure(const Text: string; Line: Integer; var Column: Integer) of object;
  TsardParsers = array[TsardScanState] of TsardParserProc;

  { TsardScanner }

  TsardScanner = class(TsardFiler)
  private
    FState: TsardScanState;
    FSection: TsardScanState;
    FCompleted: Boolean;
    FStarted: Boolean;
    FParsers: TsardParsers;
  protected
    procedure procNone(const Text: string; Line: Integer; var Column: Integer);
    procedure procHeader(const Text: string; Line: Integer; var Column: Integer);
    procedure procProlog(const Text: string; Line: Integer; var Column: Integer);
    procedure procText(const Text: string; Line: Integer; var Column: Integer);

    procedure ChangeState(NextState: TsardScanState);

    property Started:Boolean read FStarted;
    property Completed:Boolean read FCompleted;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ParseLine(const Text: string; Line: Integer);
  end;

implementation

uses
  StrUtils;

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

{ TsardScanner }

procedure TsardScanner.procNone(const Text: string; Line: Integer; var Column: Integer);
begin
  if MidStr(Text, 1, Length(sSardAnsiOpen)) = sSardAnsiOpen then
  begin
    //There is a header and it is a Ansi document
    FStarted := True;
    FCompleted := False;
    Column := Column + Length(sSardAnsiOpen); //put the column to the first char of attributes of xml document
    ChangeState(ssHeader);
  end
  else
    ChangeState(ssProlog); //nop there is no header... skip to prolog section
end;

procedure TsardScanner.procHeader(const Text: string; Line: Integer; var Column: Integer);
begin

end;

procedure TsardScanner.procProlog(const Text: string; Line: Integer; var Column: Integer);
begin

end;

procedure TsardScanner.procText(const Text: string; Line: Integer; var Column: Integer);
begin

end;

procedure TsardScanner.ChangeState(NextState: TsardScanState);
begin
  if FState <> NextState then
  begin
    //FlushBuffer(FState);
    FState := NextState;
  end;
end;

constructor TsardScanner.Create;
begin
  inherited Create;
  FSection := ssProlog;

  FParsers[ssNone] := @procNone;
  FParsers[ssHeader] := @procHeader;
  FParsers[ssText] := @procText;
  FParsers[ssProlog] := @procProlog;
end;

destructor TsardScanner.Destroy;
begin
  inherited Destroy;
end;

procedure TsardScanner.ParseLine(const Text: string; Line: Integer);
var
  Column, l: Integer;
begin
  if not Active then
    raise EsardException.Create('Scanner not started');
  Column := 1; //start of delphi string is 1
  l := Length(Text);
  while (Column <= l) do
  begin
    if not Assigned(FParsers[FState]) then
      raise EsardException.Create('Parser state not assigned');
    FParsers[FState](Text, Line, Column);
  end;
end;

end.

