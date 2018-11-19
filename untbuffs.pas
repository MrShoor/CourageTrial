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
    function Name: string; virtual; abstract;
    function Desc: string; virtual; abstract;
    function Ico : string; virtual; abstract;

    function Duration: Integer; virtual;
    function Kind: TUnitBuffKind; virtual; abstract;

    function Owner : TRoomUnit;
    function AtUnit: TRoomUnit;

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

implementation

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

