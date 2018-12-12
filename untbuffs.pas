unit untBuffs;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, untLevel, intfUtils;

type

  { TUnitBuff }

  TUnitBuff = class(TInterfacedObject, IUnitBuff)
  private
    FOwner   : IWeakRef;
    FAt      : IWeakRef;
    FDuration: Integer;
  protected
    function ID  : TUnitBuffID; virtual;
    function Name: string; virtual; abstract;
    function Desc: string; virtual; abstract;
    function Ico : string; virtual; abstract;

    function Duration: Integer; virtual;
    function Kind: TUnitBuffKind; virtual; abstract;

    function Owner : TRoomUnit;
    function AtUnit: TRoomUnit;

    procedure ProcessDamage(var ADmg: Integer; AFromUnit: TRoomUnit); virtual;

    procedure SetUnit(const AUnit: TRoomUnit);
    function DoStep: Boolean; virtual;
  public
    constructor Create(const AOwner: TRoomUnit; const ADuration: Integer);
  end;

  { TBuff_Stun }

  TBuff_Stun = class(TUnitBuff)
  private
  protected
    function Name: string; override;
    function Desc: string; override;
    function Ico : string; override;

    function Kind: TUnitBuffKind; override;
    function DoStep: Boolean; override;
  end;

  { TBuff_AbsoluteSight }

  TBuff_AbsoluteSight = class(TUnitBuff)
  private
  protected
    function ID  : TUnitBuffID; override;
    function Name: string; override;
    function Desc: string; override;
    function Ico : string; override;

    function Kind: TUnitBuffKind; override;
    function DoStep: Boolean; override;
  end;

  { TBuff_ResonantArmor }

  TBuff_ResonantArmor = class(TUnitBuff)
  private
  protected
    function ID  : TUnitBuffID; override;
    function Name: string; override;
    function Desc: string; override;
    function Ico : string; override;

    procedure ProcessDamage(var ADmg: Integer; AFromUnit: TRoomUnit); override;

    function Kind: TUnitBuffKind; override;
    function DoStep: Boolean; override;
  end;

  { TBuff_Poison }

  TBuff_Poison = class(TUnitBuff)
  private
  protected
    function ID  : TUnitBuffID; override;
    function Name: string; override;
    function Desc: string; override;
    function Ico : string; override;

    function Kind: TUnitBuffKind; override;
    function DoStep: Boolean; override;
  end;

implementation

{ TBuff_Poison }

function TBuff_Poison.ID: TUnitBuffID;
begin
  Result := bidUnknown;
end;

function TBuff_Poison.Name: string;
begin
  Result := 'Отравление';
end;

function TBuff_Poison.Desc: string;
begin
  Result := 'Каждый ход теряется 10 ОЗ';
end;

function TBuff_Poison.Ico: string;
begin
  Result := 'poison.png';
end;

function TBuff_Poison.Kind: TUnitBuffKind;
begin
  Result := bkPoison;
end;

function TBuff_Poison.DoStep: Boolean;
var unt: TRoomUnit;
begin
  Result := inherited DoStep;
  unt := Owner;
  if unt <> nil then
    unt.DealPureDamage(10, nil, unt.Name + ' получает 10 урона от яда');
end;

{ TBuff_ResonantArmor }

function TBuff_ResonantArmor.ID: TUnitBuffID;
begin
  Result := bidUnknown;
end;

function TBuff_ResonantArmor.Name: string;
begin
  Result := 'Резонирующая броня';
end;

function TBuff_ResonantArmor.Desc: string;
begin
  Result := '50% урона перенаправляется во врага';
end;

function TBuff_ResonantArmor.Ico: string;
begin
  Result := 'resonant_armor.png';
end;

procedure TBuff_ResonantArmor.ProcessDamage(var ADmg: Integer; AFromUnit: TRoomUnit);
var mirroredDmg: Integer;
begin
  mirroredDmg := ADmg div 2;
  if mirroredDmg > 0 then
  begin
    ADmg := ADmg - mirroredDmg;
    if AFromUnit <> nil then
      AFromUnit.DealDamage(mirroredDmg, nil);
  end;
end;

function TBuff_ResonantArmor.Kind: TUnitBuffKind;
begin
  Result := bkPowerUp;
end;

function TBuff_ResonantArmor.DoStep: Boolean;
begin
  Result:=inherited DoStep;
end;

{ TBuff_AbsoluteSight }

function TBuff_AbsoluteSight.ID: TUnitBuffID;
begin
  Result := bidAbsoluteSight;
end;

function TBuff_AbsoluteSight.Name: string;
begin
  Result := 'Абсолютное наблюдение';
end;

function TBuff_AbsoluteSight.Desc: string;
begin
  Result := 'Вас все видят';
end;

function TBuff_AbsoluteSight.Ico: string;
begin
  Result := 'abs_sight.png';
end;

function TBuff_AbsoluteSight.Kind: TUnitBuffKind;
begin
  Result := bkDebuff;
end;

function TBuff_AbsoluteSight.DoStep: Boolean;
begin
  Result:=inherited DoStep;
end;

{ TBuff_Stun }

function TBuff_Stun.Name: string;
begin
  Result := 'Оглушение';
end;

function TBuff_Stun.Desc: string;
begin
  Result := '';
end;

function TBuff_Stun.Ico: string;
begin
  Result := 'stun.png';
end;

function TBuff_Stun.Kind: TUnitBuffKind;
begin
  Result := bkStun;
end;

function TBuff_Stun.DoStep: Boolean;
var unt: TRoomUnit;
begin
  Result := inherited DoStep;
  unt := AtUnit;
  if unt = nil then Exit;
  if unt.AP > 0 then
    unt.Room.AddMessage(unt.Name + ' в оглушении и пропускает ход');
  unt.AP := 0;
end;

{ TUnitBuff }

function TUnitBuff.ID: TUnitBuffID;
begin
  Result := bidUnknown;
end;

function TUnitBuff.Duration: Integer;
begin
  Result := FDuration;
end;

function TUnitBuff.Owner: TRoomUnit;
begin
  Result := nil;
  if FOwner <> nil then
  begin
    Result := TRoomUnit(FOwner.Obj);
    if Result = nil then FOwner := nil;
  end;
end;

function TUnitBuff.AtUnit: TRoomUnit;
begin
  Result := nil;
  if FAt <> nil then
  begin
    Result := TRoomUnit(FAt.Obj);
    if Result = nil then FAt := nil;
  end;
end;

procedure TUnitBuff.ProcessDamage(var ADmg: Integer; AFromUnit: TRoomUnit);
begin

end;

procedure TUnitBuff.SetUnit(const AUnit: TRoomUnit);
begin
  if AUnit = nil then
    FAt := nil
  else
    FAt := AUnit.WeakRef;
end;

function TUnitBuff.DoStep: Boolean;
begin
  Dec(FDuration);
  Result := FDuration > 0;
end;

constructor TUnitBuff.Create(const AOwner: TRoomUnit; const ADuration: Integer);
begin
  if AOwner = nil then
    FOwner := nil
  else
    FOwner := AOwner.WeakRef;
  FDuration := ADuration;
end;

end.

