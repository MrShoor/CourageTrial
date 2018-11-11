unit untSkills;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, untLevel, intfUtils;

type

  { TUnitSkill }

  TUnitSkill = class(TInterfacedObject, IUnitSkill)
  private
    FItem : IWeakRefIntf;
    FIndex: Integer;
  protected
    function Item: IUnitItem;
    function Idx : Integer;
    function WearedOnly: Boolean; virtual; abstract;

    function Name: string; virtual; abstract;
    function Desc: string; virtual; abstract;
    function Ico : string; virtual; abstract;

    function Cost : Integer; virtual; abstract;
    function Range: Single; virtual; abstract;
    function Animation: string; virtual; abstract;

    function DoAction(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit): IBRA_Action; virtual; abstract;
    function CanUse(ASkillIndex: Integer; AOwner, ATarget: TRoomUnit; AReservedPoints: Integer = 0): Boolean; virtual; abstract;
  public
    constructor Create(const AItem: IUnitItem; const AIndex: Integer);
  end;

  { TSkill_Kick }

  TSkill_Kick = class(TUnitSkill)
  protected
    function WearedOnly: Boolean; override;

    function Name: string; override;
    function Desc: string; override;
    function Ico : string; override;

    function Cost : Integer; override;
    function Range: Single; override;
    function Animation: string; override;
  end;

implementation

{ TUnitSkill }

function TUnitSkill.Item: IUnitItem;
begin
  if FItem = nil then Exit(nil);
  Result := FItem.Intf as IUnitItem;
  if Result = nil then FItem := nil;
end;

function TUnitSkill.Idx: Integer;
begin
  Result := FIndex;
end;

constructor TUnitSkill.Create(const AItem: IUnitItem; const AIndex: Integer);
begin
  FItem := (AItem as IWeakedInterface).WeakRef;
  FIndex := AIndex;
end;

{ TSkill_Kick }

function TSkill_Kick.WearedOnly: Boolean;
begin
  Result := False;
end;

function TSkill_Kick.Name: string;
begin
  Result := 'Пинок ногой';
end;

function TSkill_Kick.Desc: string;
begin
  Result := 'Всегда ведь приятно пнуть врага';
end;

function TSkill_Kick.Ico: string;
begin
  Result := 'ui\skills\kick.png';
end;

function TSkill_Kick.Cost: Integer;
begin
  Result := 3;
end;

function TSkill_Kick.Range: Single;
begin
  Result := 1;
end;

function TSkill_Kick.Animation: string;
begin
  Result := 'Kick0';
end;

end.

