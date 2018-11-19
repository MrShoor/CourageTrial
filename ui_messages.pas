unit ui_messages;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avBase, avRes, avMiniControls, avCanvas, mutils, avTypes, ui_scroll, intfUtils, avContnrs;

type
  { TavmMessages }

  TavmMessages = class(TavmCustomControl)
  private const
    textXSpacing = 10;
  private type
    TRoomMessage = record
      str: string;
      txt: ITextLines;
    end;
    IRoomMessageArr = {$IfDef FPC}specialize{$EndIf} IArray<TRoomMessage>;
    TRoomMessageArr = {$IfDef FPC}specialize{$EndIf} TArray<TRoomMessage>;
  private
    FMessages: IRoomMessageArr;
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
  public
    procedure AddMessage(const AStr: string);
  end;

implementation

{ TavmMessages }

procedure TavmMessages.AfterRegister;
begin
  inherited AfterRegister;
  FMessages := TRoomMessageArr.Create();
  Size := Vec(400, 400);
end;

procedure TavmMessages.DoValidate;
const
  cMessageSpacing = 20;
var
  i: Integer;
  y: Single;
begin
  inherited DoValidate;

  Canvas.Pen.Color := Vec(0,0,0,1);
  Canvas.Brush.Color := Vec(0.125,0.125,0.125,1);

  Canvas.Clear;
  Canvas.AddFill(Vec(0,0), Size);
  Canvas.AddRectangle(Vec(0,0), Size);

  y := 0;
  for i := FMessages.Count - 1 downto 0 do
  begin
    FMessages[i].txt.VScroll := y;
    Canvas.AddText(FMessages[i].txt);
    y := y - FMessages[i].txt.TotalHeight - cMessageSpacing;
    if y < -Size.y then Break;
  end;
end;

procedure TavmMessages.AddMessage(const AStr: string);
var newMsg: TRoomMessage;
begin
  newMsg.str := AStr;

  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := 20;
  Canvas.Font.Style:= [];

  with Canvas.TextBuilder do
  begin
    WriteWrapped(AStr);
    WriteWrappedEnd(Size.x - textXSpacing*2, True, 20);
    newMsg.txt := Finish();
    newMsg.txt.VAlign := 1;
    newMsg.txt.ClipWithBounds := True;
    newMsg.txt.BoundsX := Vec(textXSpacing, Size.x - textXSpacing);
    newMsg.txt.BoundsY := Vec(textXSpacing, Size.y - textXSpacing);
  end;

  FMessages.Add(newMsg);
  Invalidate;
end;

end.

