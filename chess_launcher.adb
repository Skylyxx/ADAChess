with Ada.Text_IO;
use Ada.Text_IO;

with Menu;

with Chess_Game;
use Chess_Game;

procedure Chess_Launcher is
	Game : T_Game;
begin

	-- Menu intéractif
	case Menu.Wait_For_Input is
		when Menu.New_Game =>
			Put_Line("New Game !");
			Init_Game(Game);
			Next_Turn(Game);
		when Menu.Load_Game =>
			Put_Line("Load game !");
			-- Load_Game;
		when others =>
			Put_Line("Bye !");
	end case;

end Chess_Launcher;