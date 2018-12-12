unit untItems;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, untLevel, intfUtils, mutils;

type
  { TUnitItem }

  TUnitItem = class(TWeakedInterfacedObject, IUnitItem)
  protected
    FEquipped: Boolean;
    FSkills: array of IUnitSkill;

    function CheckConsume(AUnit: TRoomUnit): Boolean;
  public
    function  GetEquipped: Boolean;
    procedure SetEquipped(const AValue: Boolean);

    function Name  : string;          virtual; abstract;
    function Kind  : TUnitItemKind;   virtual; abstract;
    function Slot  : TRoomUnitEqSlot; virtual; abstract;
    function Model : string;          virtual; abstract;
    function Ico48 : string;          virtual; abstract;

    function Weapon_Damage: TVec2i; virtual;
    function ExtraDesc: string; virtual;

    function SkillsCount: Integer; virtual;
    function Skill(ASkillIndex: Integer): IUnitSkill; virtual;

    function Consume(AUnit: TRoomUnit): IBRA_Action; virtual;

    procedure ProcessDamage(ADmg: Integer; AFromUnit: TRoomUnit); virtual;

    constructor Create; virtual;
  end;

  { TArcherBow }

  TArcherBow = class(TUnitItem)
  public
    function Name  : string;          override;
    function Kind  : TUnitItemKind;   override;
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;

    function Weapon_Damage: TVec2i; override;
  end;

  { TAxe }

  TAxe = class(TUnitItem)
  public
    function Name  : string;          override;
    function Kind  : TUnitItemKind;   override;
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;

    function Weapon_Damage: TVec2i; override;
  end;

  { TPoisonBottle }

  TPoisonBottle = class(TUnitItem)
  private
  public
    function Name  : string;          override;
    function Kind  : TUnitItemKind;   override;
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;

    function ExtraDesc: string; override;

    function Consume(AUnit: TRoomUnit): IBRA_Action; override;
  end;

  { THealBottle }

  THealBottle = class(TUnitItem)
  private
    FHealEff: Integer;
  public
    function Name  : string;          override;
    function Kind  : TUnitItemKind;   override;
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;

    function ExtraDesc: string; override;

    function Consume(AUnit: TRoomUnit): IBRA_Action; override;
    constructor Create; override;
  end;

  { THealBottle2 }

  THealBottle2 = class(THealBottle)
  public
    function Ico48 : string; override;
    constructor Create; override;
  end;

  { TScroll_ResonantArmor }

  TScroll_ResonantArmor = class(TUnitItem)
  private
  public
    function Name  : string;          override;
    function Kind  : TUnitItemKind;   override;
    function Slot  : TRoomUnitEqSlot; override;
    function Model : string;          override;
    function Ico48 : string;          override;

    function ExtraDesc: string; override;

    function Consume(AUnit: TRoomUnit): IBRA_Action; override;
    constructor Create; override;
  end;

implementation

uses
  Math, untBuffs;

type
  TBRA_DrinkPotion = class(TBRA_Action)
  private
    FUnit: TRoomUnit;
    FStopTime: Int64;
  public
    function ProcessAction: Boolean; override;
    constructor Create(AUnit: TRoomUnit; const AItem: IUnitItem);
  end;

  TBRA_UseBuffScroll = class(TBRA_Action)
  private
    FUnit: TRoomUnit;
    FStopTime: Int64;
  public
    function ProcessAction: Boolean; override;
    constructor Create(AUnit: TRoomUnit; const AItem: IUnitItem; const ABuff: IUnitBuff);
  end;

{ TPoisonBottle }

function TPoisonBottle.Name: string;
begin
  Result := 'Яд?';
end;

function TPoisonBottle.Kind: TUnitItemKind;
begin
  Result := ikConsumable;
end;

function TPoisonBottle.Slot: TRoomUnitEqSlot;
begin
  Result := esNone;
end;

function TPoisonBottle.Model: string;
begin
  Result := 'PotionB';
end;

function TPoisonBottle.Ico48: string;
begin
  Result := 'poison.png';
end;

function TPoisonBottle.ExtraDesc: string;
begin
  Result := 'Жизнь слишком проста? Ну ладно, пей';
end;

function TPoisonBottle.Consume(AUnit: TRoomUnit): IBRA_Action;
begin
  if not CheckConsume(AUnit) then Exit(nil);
  AUnit.AP := AUnit.AP - 1;
  Result := TBRA_DrinkPotion.Create(AUnit, Self);
  if Result <> nil then
  begin
    AUnit.Room.AddMessage(AUnit.Name + ' выпивает неизвестную жидкость, и чувствует, как яд расходится по венам.');
    AUnit.ApplyBuff(TBuff_Poison.Create(AUnit, 5));
    AUnit.Inventory().Pop(self);
  end;
end;

{ THealBottle2 }

function THealBottle2.Ico48: string;
begin
  Result := 'potion_hp2.png';
end;

constructor THealBottle2.Create;
begin
  inherited Create;
  FHealEff := 50;
end;

{ TBRA_UseBuffScroll }

function TBRA_UseBuffScroll.ProcessAction: Boolean;
begin
  Result := FUnit.World.GameTime < FStopTime;
end;

constructor TBRA_UseBuffScroll.Create(AUnit: TRoomUnit; const AItem: IUnitItem; const ABuff: IUnitBuff);
begin
  FUnit := AUnit;
  FUnit.AP := FUnit.AP - 1;
  FUnit.Inventory().Pop(AItem);
  FUnit.SetAnimation(['SelfCast']);
  FUnit.ApplyBuff(ABuff);
  FStopTime := FUnit.World.GameTime + 1500;
end;


{ TScroll_ResonantArmor }

function TScroll_ResonantArmor.Name: string;
begin
  Result := 'Свиток резонирующей брони';
end;

function TScroll_ResonantArmor.Kind: TUnitItemKind;
begin
  Result := ikConsumable;
end;

function TScroll_ResonantArmor.Slot: TRoomUnitEqSlot;
begin
  Result := esNone;
end;

function TScroll_ResonantArmor.Model: string;
begin
  Result := 'Scroll';
end;

function TScroll_ResonantArmor.Ico48: string;
begin
  Result := 'scroll_resonant_armor.png';
end;

function TScroll_ResonantArmor.ExtraDesc: string;
begin
  Result := 'Перенаправляет 50% урона обратно во врага';
end;

function TScroll_ResonantArmor.Consume(AUnit: TRoomUnit): IBRA_Action;
begin
  if not CheckConsume(AUnit) then Exit(nil);
  Result := TBRA_UseBuffScroll.Create(AUnit, Self, TBuff_ResonantArmor.Create(AUnit, 8));
end;

constructor TScroll_ResonantArmor.Create;
begin
  inherited Create;
end;

{ TBRA_DrinkPotion }

function TBRA_DrinkPotion.ProcessAction: Boolean;
begin
  Result := FUnit.World.GameTime < FStopTime;
  if not Result then
    FUnit.TemporaryUnEquip(esLeftHand);
end;

constructor TBRA_DrinkPotion.Create(AUnit: TRoomUnit; const AItem: IUnitItem);
begin
  FUnit := AUnit;
  FUnit.TemporaryEquip(esLeftHand, AItem.Model);
  FUnit.SetAnimation(['Drink']);
  FStopTime := FUnit.World.GameTime + 1000;
end;

{ THealBottle }

function THealBottle.Name: string;
begin
  Result := 'Зелье здоровья';
end;

function THealBottle.Kind: TUnitItemKind;
begin
  Result := ikConsumable;
end;

function THealBottle.Slot: TRoomUnitEqSlot;
begin
  Result := esNone;
end;

function THealBottle.Model: string;
begin
  Result := 'PotionA';
end;

function THealBottle.Ico48: string;
begin
  Result := 'potion_hp.png';
end;

function THealBottle.ExtraDesc: string;
begin
  Result := 'Восстанавливает '+IntToStr(FHealEff)+ ' ОЗ';
end;

function THealBottle.Consume(AUnit: TRoomUnit): IBRA_Action;
begin
  if not CheckConsume(AUnit) then Exit(nil);
  AUnit.AP := AUnit.AP - 1;
  Result := TBRA_DrinkPotion.Create(AUnit, Self);
  if Result <> nil then
  begin
    AUnit.HP := Min(AUnit.HP + FHealEff, AUnit.MaxHP);
    AUnit.Inventory().Pop(self);
  end;
end;

constructor THealBottle.Create;
begin
  inherited Create;
  FHealEff := 30;
end;

{ TAxe }

function TAxe.Name: string;
begin
  Result := 'Топорик';
end;

function TAxe.Kind: TUnitItemKind;
begin
  Result := ikAxe;
end;

function TAxe.Slot: TRoomUnitEqSlot;
begin
  Result := TRoomUnitEqSlot.esRightHand;
end;

function TAxe.Model: string;
begin
  Result := 'Axe0';
end;

function TAxe.Ico48: string;
begin
  Result := 'axe.png';
end;

function TAxe.Weapon_Damage: TVec2i;
begin
  Result := Vec(30, 40);
end;

{ TUnitItem }

function TUnitItem.CheckConsume(AUnit: TRoomUnit): Boolean;
var
  n: Integer;
begin
  n := AUnit.Inventory().Items.IndexOf(Self);
  if n < 0 then Exit(False);

  if AUnit.AP < 1 then
  begin
    AUnit.Room.AddMessage('Требуется 1 очко действий.');
    Exit(False);
  end;
  Result := True;
end;

function TUnitItem.GetEquipped: Boolean;
begin
  Result := FEquipped;
end;

procedure TUnitItem.SetEquipped(const AValue: Boolean);
begin
  if Slot = esNone then Exit;
  FEquipped := AValue;
end;

function TUnitItem.Weapon_Damage: TVec2i;
begin
  Result := Vec(0,0);
end;

function TUnitItem.ExtraDesc: string;
begin
  Result := '';
end;

function TUnitItem.SkillsCount: Integer;
begin
  Result := Length(FSkills);
end;

function TUnitItem.Skill(ASkillIndex: Integer): IUnitSkill;
begin
  Result := FSkills[ASkillIndex];
end;

function TUnitItem.Consume(AUnit: TRoomUnit): IBRA_Action;
begin
  Result := nil;
end;

procedure TUnitItem.ProcessDamage(ADmg: Integer; AFromUnit: TRoomUnit);
begin

end;

constructor TUnitItem.Create;
begin

end;

{ TArcherBow }

function TArcherBow.Name: string;
begin
  Result := 'Лук';
end;

function TArcherBow.Kind: TUnitItemKind;
begin
  Result := ikBow;
end;

function TArcherBow.Slot: TRoomUnitEqSlot;
begin
  Result := esBothHands;
end;

function TArcherBow.Model: string;
begin
  Result := 'Archer_Bow';
end;

function TArcherBow.Ico48: string;
begin
  Result := 'bow.png';
end;

function TArcherBow.Weapon_Damage: TVec2i;
begin
  Result := Vec(10, 30);
end;

end.

