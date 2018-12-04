unit untRoomObstacles;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, Graphics, SysUtils,
  avBase,
  untLevel, mutils, bWorld, bLights, untObstacles;

type

  { TPointLight_Props }

  TPointLight_Props = class(TPersistent)
  private
    FOnChange: TNotifyEvent;

    FCastShadow: Boolean;
    FColor: TColor;
    FEnabled: Boolean;
    FIntensity: Single;
    FRadius: Single;

    procedure DoOnChange();
    procedure SetCastShadow(const AValue: Boolean);
    procedure SetColor(const AValue: TColor);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetIntensity(const AValue: Single);
    procedure SetRadius(const AValue: Single);
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Color: TColor read FColor write SetColor;
    property Radius: Single read FRadius write SetRadius;
    property Intensity: Single read FIntensity write SetIntensity;
    property CastShadow: Boolean read FCastShadow write SetCastShadow;

    constructor Create(const AOnChange: TNotifyEvent);
  end;


  { TLightSource }

  TLightSource = class(TObstacle)
  protected
    FLight: IavPointLight;
    FProps: TPointLight_Props;
    procedure PropsChanged(ASender: TObject);
  protected
    function LightOffset(): TVec3; virtual; abstract;

    procedure SetRoomDir(const AValue: Integer); override;
    procedure SetRoomPos(const AValue: TVec2i); override;
  public
    procedure LoadComplete; override;
  public
    function GetProps: TPersistent; override;

    procedure LoadModels(const AObstacle: TObstacleDesc); override;
    procedure SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean = True); override;
  public
    constructor Create(AParent: TavObject); override;
    destructor Destroy; override;
  end;

  { TLantern }

  TLantern = class (TLightSource)
  protected
    function LightOffset(): TVec3; override;
  end;

  { TBrazier }

  TBrazier = class (TLightSource)
  protected
    function LightOffset(): TVec3; override;
  end;

  { TTorchStand }

  TTorchStand = class (TLightSource)
  protected
    function LightOffset(): TVec3; override;
  end;

implementation

{ TPointLight_Props }

procedure TPointLight_Props.DoOnChange();
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPointLight_Props.SetCastShadow(const AValue: Boolean);
begin
  if FCastShadow = AValue then Exit;
  FCastShadow := AValue;
  DoOnChange();
end;

procedure TPointLight_Props.SetColor(const AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  DoOnChange();
end;

procedure TPointLight_Props.SetEnabled(const AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  DoOnChange();
end;

procedure TPointLight_Props.SetIntensity(const AValue: Single);
begin
  if FIntensity = AValue then Exit;
  FIntensity := AValue;
  DoOnChange();
end;

procedure TPointLight_Props.SetRadius(const AValue: Single);
begin
  if FRadius = AValue then Exit;
  FRadius := AValue;
  DoOnChange();
end;

constructor TPointLight_Props.Create(const AOnChange: TNotifyEvent);
var vb: TVec4b;
begin
  vb.x := 255;
  vb.y := 167;
  vb.z := 0;
  vb.w := 0;

  FIntensity := 18;
  FRadius := 10;
  FColor := TColor(vb);
  FOnChange := AOnChange;
  FCastShadow := True;
  FEnabled := True;
end;

{ TTorchStand }

function TTorchStand.LightOffset(): TVec3;
begin
  Result := Vec(0, 1.72562, 0);
end;

{ TBrazier }

function TBrazier.LightOffset(): TVec3;
begin
  Result := Vec(0, 1, 0);
end;

{ TLightSource }

procedure TLightSource.PropsChanged(ASender: TObject);

  function ConvertColor(c: TColor): TVec3;
  var vb: TVec4b absolute c;
  begin
    Result := Vec(vb.x, vb.y, vb.z) / 255.0;
  end;

begin
  if not FProps.Enabled then
  begin
    FLight := nil;
  end
  else
  begin
    if FLight = nil then FLight := World.Renderer.CreatePointLight();
    FLight.Pos := LightOffset() * Transform();
    FLight.Radius := FProps.Radius;
    FLight.Size := 0.1;
    //FLight.Color := Vec(1,1,1.0)*4.41;
    //FLight.Color := Pow(Vec(1,0.655,0),1.0)*4.41*4;
    FLight.Color := ConvertColor(FProps.Color)*FProps.Intensity;
    if FProps.CastShadow then
      FLight.CastShadows := st512
    else
      FLight.CastShadows := stNone;
    FLight.Auto_ShadowRenderType := True;
  end;
  Main.InvalidateWindow;
end;

procedure TLightSource.SetRoomDir(const AValue: Integer);
begin
  inherited SetRoomDir(AValue);
  if FLight <> nil then
    FLight.Pos := LightOffset() * Transform();
end;

procedure TLightSource.SetRoomPos(const AValue: TVec2i);
begin
  inherited SetRoomPos(AValue);
  if FLight <> nil then
    FLight.Pos := LightOffset() * Transform();
end;

procedure TLightSource.LoadComplete;
begin
  inherited LoadComplete;
  PropsChanged(FProps);
end;

function TLightSource.GetProps: TPersistent;
begin
  Result := FProps;
end;

procedure TLightSource.LoadModels(const AObstacle: TObstacleDesc);
begin
  inherited LoadModels(AObstacle);
  PropsChanged(FProps);
end;

procedure TLightSource.SetRoomPosDir(const APos: TVec2i; const ADir: Integer; const AAutoRegister: Boolean);
begin
  inherited SetRoomPosDir(APos, ADir, AAutoRegister);
  FLight.Pos := LightOffset() * Transform();
end;

constructor TLightSource.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  FProps := TPointLight_Props.Create({$IfDef FPC}@{$EndIf}PropsChanged);
end;

destructor TLightSource.Destroy;
begin
  FreeAndNil(FProps);
  inherited Destroy;
end;

{ TLantern }

function TLantern.LightOffset(): TVec3;
begin
  Result := Vec(0.89635, 2.51368, 0.03713);
end;

initialization
  RegRoomClass(TLantern);
  RegRoomClass(TBrazier);
  RegRoomClass(TTorchStand);

end.

