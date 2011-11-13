unit welcframe; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ActnList;

type

  { TWelcomeFrame }

  TWelcomeFrame = class(TFrame)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    BackendList: TListBox;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ActCalc, ActMulti, ActCal: TAction;

implementation

{$R *.lfm}

{ TWelcomeFrame }

procedure TWelcomeFrame.Button1Click(Sender: TObject);
begin
  case BackendList.ItemIndex of
  0: ActCalc.Execute;
  1: ActMulti.Execute;
  2: ActCal.Execute;
  end;
end;

end.

