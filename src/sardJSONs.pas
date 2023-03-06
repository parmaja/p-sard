unit sardJSONs;
{**
*  This file is part of the "SARD"
*
* @license   The MIT License (MIT)
*            Included in this distribution
* @author    Zaher Dirkey 
*}

{
  bugs:
}

{$IFDEF FPC}
{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  mnUtils, mnClasses, mnDON,
  sardClasses, sardParsers, sardStandards;

type
  TJSONParser = class;

  { TJSONController }

  TJSONController = class(TController)
  protected
  public
    procedure SetControl(Control: TSardControl); override;
  end;

  { TJSONCollector }

  TJSONCollector = class(TCollector)
  private
    function GetParser: TJSONParser;
  protected
    procedure DoControl(AControl: TSardControl); override;
  public
    function IsInitial: Boolean; override;
    procedure Reset; override;
    property Parser: TJSONParser read GetParser;
  end;

  { TJSONCollector_Value }

  TJSONCollector_Value = class(TJSONCollector)
  private
  protected
    type
      TExpect = (valValue, valNext);
    var
      Expect: TExpect;
    CurrentObject: TObject;
    Name: string;
  public
    constructor Create(AParser: TParser; const AName: string; AObject: TObject);
    procedure Reset; override;
    procedure DoToken(Token: TSardToken); override;
    procedure DoControl(AControl: TSardControl); override;
  end;

  { TJSONCollector_Pair }

  {
    Pair is Name and Value, Value can be String/Object/Array
  }

  TJSONCollector_Pair = class(TJSONCollector)
  private
    Name: string;
  protected
    type
      TExpect = (elmName, elmValue);
    var
      Expect: TExpect;
    ParentPair: TObject;
    CurrentObject: TObject;
  public
    constructor Create(AParser: TParser; AParentPair: TObject; ACurrentObject: TObject = nil); overload;
    procedure Reset; override;
    procedure DoToken(Token: TSardToken); override;
    procedure DoControl(AControl: TSardControl); override;
  end;

  { TJSONCollector_Array }

  TJSONCollector_Array = class(TJSONCollector)
  private
  protected
    type
      TExpect = (aryValue, aryNext);
    var
      Expect: TExpect;
    ParentPair: TObject; //element with array value created
    Name: string;
  public
    constructor Create(AParser: TParser; const AName: string; AParentObject: TObject);
    procedure Reset; override;
    procedure DoToken(Token: TSardToken); override;
    procedure DoControl(AControl: TSardControl); override;
  end;

  { TJSONLexer }

  TJSONLexer = class(TLexer)
  protected
    const
      sNumberOpenChars = ['-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
      sNumberChars = sNumberOpenChars + ['.', 'x', 'h', 'a', 'b', 'c', 'd', 'e', 'f'];
  public
    constructor Create; override;
    function IsNumber(const vChar: Char; vOpen: Boolean =true): Boolean; override;
    function IsIdentifier(const vChar: Char; vOpen: Boolean =true): Boolean;
  end;

  { TJSONParser }

  TJSONParser = class abstract(TParser)
  private
    FStrict: Boolean;
  protected
    Lexer: TLexer;
    procedure AcquirePair(AParentObject: TObject; const AName: string; out AObject: TObject); virtual;
    function AcquireValue(AParentObject: TObject; const AName: string; const AValue: string; AType: TDONType): TObject; virtual; abstract;
  public
    constructor Create(ALexer: TLexer; AObject: TObject); overload; virtual;
    destructor Destroy; override;

    property Strict: Boolean read FStrict write FStrict default true;
  end;

  TJSONParserClass = class of TJSONParser;

//-----------------------------------------------------------------------------

 { TDataJSONParser }

  TDataJSONParser = class(TJSONParser)
  protected
  protected
    procedure AcquirePair(AParentObject: TObject; const AJSONName: string; out AJSONObject: TObject); override;
    function AcquireValue(AParentObject: TObject; const AName: string; const AValue: string; AType: TDONType): TObject; override;
  public
  end;

  { TJSONScanner }

  TJSONScanner = class(TScanner)
  private
    FStrict: Boolean;
  protected
    FParserClass: TJSONParserClass;
    FRoot: TObject;
    function CreateParser: TParser; override;
  public
    constructor Create(ARoot: TObject; AParserClass: TJSONParserClass);
    procedure Compile(Lines: TStringList); overload;
    procedure Compile(const Text: string); overload;
    procedure CompileStream(const Stream: TStream); overload;
    procedure CompileFile(const FileName: string); overload;
    property Root: TObject read FRoot;
    property Strict: Boolean read FStrict write FStrict default true;
  end;

implementation

uses
  StrUtils;

{ TJSONCollector_Array }

constructor TJSONCollector_Array.Create(AParser: TParser; const AName: string; AParentObject: TObject);
begin
  inherited Create(AParser);
  Name := AName;
  ParentPair := AParentObject;
end;

procedure TJSONCollector_Array.Reset;
begin
  inherited Reset;
  Expect := Low(Expect);
end;

procedure TJSONCollector_Array.DoToken(Token: TSardToken);
begin
  RaiseError('No token for array!');
end;

procedure TJSONCollector_Array.DoControl(AControl: TSardControl);
begin
  case AControl.Code of
    ctlOpenArray:
    begin
      Parser.Push(TJSONCollector_Value.Create(Parser, Name, ParentPair));
    end;
    ctlCloseArray:
    begin
      Parser.SetAction([paPop]);
      Reset;
    end;
    ctlNext:
    begin
      Parser.Push(TJSONCollector_Value.Create(Parser, Name, ParentPair));
      Reset;
    end;
    else
      inherited;
  end;
end;

{ TJSONCollector_Value }

constructor TJSONCollector_Value.Create(AParser: TParser; const AName: string; AObject: TObject);
begin
  inherited Create(AParser);
  if AObject = nil then
    RaiseError('Object nil in collection value!');
  Name := AName;
  CurrentObject := AObject;
end;

procedure TJSONCollector_Value.Reset;
begin
  inherited Reset;
  Expect := Low(Expect);
end;

procedure TJSONCollector_Value.DoToken(Token: TSardToken);
begin
  if Expect = valValue then
  begin
    if (Token.TokenType = typeString) then
      Parser.AcquireValue(CurrentObject, Name, Token.Value, donString)
    else if (Token.TokenType = typeIdentifier) then
    begin
      if (Token.Value = 'true') or (Token.Value = 'false') then
        Parser.AcquireValue(CurrentObject, Name, Token.Value, donBoolean)
      else
        Parser.AcquireValue(CurrentObject, Name, Token.Value, donIdentifier)
    end
    else if (Token.TokenType = typeNumber) then
      Parser.AcquireValue(CurrentObject, Name, Token.Value, donNumber);
    Inc(Expect);
  end
  else
    RaiseError('Value not expected: ' + Token.Value);
end;

procedure TJSONCollector_Value.DoControl(AControl: TSardControl);
begin
  case AControl.Code of
    ctlOpenBlock:
    begin
      if Expect = valValue then
      begin
        Parser.Push(TJSONCollector_Pair.Create(Parser, CurrentObject, Parser.AcquireValue(CurrentObject, Name, '', donObject)));
        Inc(Expect);
      end
      else
        inherited;
    end;
    ctlCloseBlock:
    begin
      if (Expect = valNext) or not Parser.Strict then
      begin
        Parser.SetAction([paPass, paPop]);
      end
      else
        inherited;
    end;
    ctlOpenArray:
    begin
      if Expect = valValue then
      begin
        Parser.Push(TJSONCollector_Array.Create(Parser, Name, Parser.AcquireValue(CurrentObject, Name, '', donArray)));
        Parser.SetAction([paPass]);
        Inc(Expect);
      end
      else
        inherited;
    end;
    ctlCloseArray:
    begin
      Parser.SetAction([paPass, paPop]);//pass it to parent
    end;
    ctlStop: //ctlStop using stop if file ended after value, we treat it as comma, but in Element collector we will check if that error
    begin
      if (Expect = valNext) or not Parser.Strict then
      begin
        Parser.SetAction([paPass, paPop]);
        Reset;
      end
      else
        inherited;
    end;
    ctlNext:
    begin
      if (Expect = valNext) or not Parser.Strict then
      begin
        Parser.SetAction([paPass, paPop]); //paPass pass it to array to push another value collection, but element should ignore it
        Reset;
      end
      else
        inherited;
    end;
    else
      inherited;
  end;
end;

{ TJSONCollector_Pair }

constructor TJSONCollector_Pair.Create(AParser: TParser; AParentPair: TObject; ACurrentObject: TObject);
begin
  inherited Create(AParser);
  ParentPair := AParentPair;
  CurrentObject := ACurrentObject;
end;

procedure TJSONCollector_Pair.Reset;
begin
  inherited Reset;
  Name := '';
  Expect := Low(Expect);
end;

procedure TJSONCollector_Pair.DoToken(Token: TSardToken);
begin
  if (Expect = elmName) and ((Token.TokenType = typeIdentifier) or (Token.TokenType = typeString)) then
  begin
    if Name <> '' then
      RaiseError('Name already set: ' + Name);
    Name := Token.Value;
    //Inc(Expect);
  end
  else
    RaiseError('Name expected');
end;

procedure TJSONCollector_Pair.DoControl(AControl: TSardControl);
var
  AObject: TObject;
begin
  case AControl.Code of
    ctlStart:
    begin
      if Expect = elmName then
      begin
        Parser.Push(TJSONCollector_Value.Create(Parser, Name, ParentPair));
      end
      else
        inherited;
    end;

    ctlAssign:
    begin
      if Expect = elmName then
      begin
        Parser.AcquirePair(CurrentObject, Name, AObject);
        Parser.Push(TJSONCollector_Value.Create(Parser, Name, AObject));
        Inc(Expect);//To Value
      end
      else
        inherited;
    end;

    ctlCloseBlock:
    begin
      if (Expect = elmValue) or not Parser.Strict then
      begin
        Parser.SetAction([paPop]);
        Reset;
      end
      else
        inherited;
    end;

    ctlNext:
    begin
      if Expect = elmValue then
        Reset //good if it comes from values of array
      else
        inherited;
    end;
    else
      inherited;
  end;
end;

{ TJSONCollector }

function TJSONCollector.GetParser: TJSONParser;
begin
  Result := (inherited Parser) as TJSONParser;
end;

procedure TJSONCollector.DoControl(AControl: TSardControl);
begin
  case AControl.Code of
    ctlStart: ;
    ctlStop:
    begin
      if (Parser.Count > 1) and Parser.Strict then
        RaiseError('Incomplete Code');
    end
    else
      RaiseError('Not Expected ' + AControl.Name);
  end;
end;

function TJSONCollector.IsInitial: Boolean;
begin
  Result := false;
end;

procedure TJSONCollector.Reset;
begin
end;

procedure TJSONController.SetControl(Control: TSardControl);
begin
  with (Collector as TJSONCollector) do
  begin
  end;
end;

{ TJSONScanner }

function TJSONScanner.CreateParser: TParser;
begin
  Result := FParserClass.Create(Current, FRoot);
  (Result as TJSONParser).Strict := Strict;
end;

procedure TJSONScanner.Compile(Lines: TStringList);
begin
  Scan(Lines);
end;

procedure TJSONScanner.Compile(const Text: string);
begin
  Scan(Text);
end;

procedure TJSONScanner.CompileFile(const FileName: string);
begin
  ScanFile(FileName);
end;

procedure TJSONScanner.CompileStream(const Stream: TStream);
begin
  Scan(Stream);
end;

constructor TJSONScanner.Create(ARoot: TObject; AParserClass: TJSONParserClass);
begin
  inherited Create;
  FStrict := True;
  FRoot := ARoot;
  FParserClass := AParserClass;
  Add(TJSONLexer.Create);
end;

{ TJSONParser }

procedure TJSONParser.AcquirePair(AParentObject: TObject; const AName: string; out AObject: TObject);
begin
  AObject := nil;
end;

constructor TJSONParser.Create(ALexer: TLexer; AObject: TObject);
begin
  inherited Create(False); //TODO check if own
  FStrict := True;
  Lexer := ALexer;
  Push(TJSONCollector_Pair.Create(Self, AObject));
end;

destructor TJSONParser.Destroy;
begin
  inherited;
end;

{ TDataJSONParser }

procedure TDataJSONParser.AcquirePair(AParentObject: TObject; const AJSONName: string; out AJSONObject: TObject);
begin
  if not (AParentObject is TDON_Object_Value) then
    RaiseError('Value is not object');
  (AParentObject as TDON_Object_Value).AcquirePair(AJSONName, AJSONObject);
end;

function TDataJSONParser.AcquireValue(AParentObject: TObject; const AName: string; const AValue: string; AType: TDONType): TObject;
begin
  Result := donAcquireValue(AParentObject, AValue, AType);
end;

{ TJSONLexer }

constructor TJSONLexer.Create;
begin
  inherited;

  Add(TWhitespace_Tokenizer.Create);
  Add(TNumber_Tokenizer.Create);
  Add(TSL_DQ_String_Tokenizer.Create);
  //Add(TML_Comment_Tokenizer.Create);
  //Add(TSL_Comment_Tokenizer.Create);
  Add(TControl_Tokenizer.Create('(', ctlOpenParams));
  Add(TControl_Tokenizer.Create('[', ctlOpenArray));
  Add(TControl_Tokenizer.Create('{', ctlOpenBlock));
  Add(TControl_Tokenizer.Create(')', ctlCloseParams));
  Add(TControl_Tokenizer.Create(']', ctlCloseArray));
  Add(TControl_Tokenizer.Create('}', ctlCloseBlock));
  Add(TControl_Tokenizer.Create(',', ctlNext));
  Add(TControl_Tokenizer.Create(':', ctlAssign));

  Add(TIdentifier_Tokenizer.Create);//Sould be last one
end;

function TJSONLexer.IsNumber(const vChar: Char; vOpen: Boolean): Boolean;
begin
  if (vOpen) then
    Result := CharInSet(vChar, sNumberOpenChars)
  else
    Result := CharInSet(vChar, sNumberChars);
end;

function TJSONLexer.IsIdentifier(const vChar: Char; vOpen: Boolean): Boolean;
begin
  Result := inherited isIdentifier(vChar, vOpen); //we do not need to override it, but it is nice to see it here
end;

end.
