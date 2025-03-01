with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Characters.Latin_1;
use Ada.Text_IO, Ada.Strings.Unbounded, Ada.Characters.Latin_1;

package body Menu is
	
	-- Effacer l'écran et afficher le header
	procedure Display_Header is
	begin
		for I in 0..50 loop
			New_Line;
		end loop;
		
		Put_Line(ESC & "[33m         █████╗ ██████╗  █████╗         ");
		Put_Line("        ██╔══██╗██╔══██╗██╔══██╗        ");
		Put_Line("        ███████║██║  ██║███████║        ");
		Put_Line("        ██╔══██║██║  ██║██╔══██║        ");
		Put_Line("        ██║  ██║██████╔╝██║  ██║        ");
		Put_Line("        ╚═╝  ╚═╝╚═════╝ ╚═╝  ╚═╝        ");
		Put_Line("                                        ");
		Put_Line(ESC & "[32m ██████╗██╗  ██╗███████╗███████╗███████╗");
		Put_Line("██╔════╝██║  ██║██╔════╝██╔════╝██╔════╝");
		Put_Line("██║     ███████║█████╗  ███████╗███████╗");
		Put_Line("██║     ██╔══██║██╔══╝  ╚════██║╚════██║");
		Put_Line("╚██████╗██║  ██║███████╗███████║███████║");
		Put_Line(" ╚═════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝" & ESC & "[0m");
	end Display_Header;

	procedure Display_Menu is
	begin
		Display_Header;
		
		New_Line;
		Put_Line("Veuillez choisir une option parmis les suivantes");
		New_Line;

		for I in Menu_Label'Range loop
			if I = Current_Selection then
				Put_Line(ESC & "[31m> " & To_String(Menu_Label(I)) & " <" & ESC & "[0m");
			else
				Put_Line(ESC & "[36m" & To_String(Menu_Label(I)) & ESC & "[0m");
			end if;
		end loop;
	end Display_Menu;

	function Wait_For_Input return Menu_Item is
		Key : Character;
	begin
		loop
			Display_Menu;
			Get_Immediate(Key);
			if Key = 'A' then -- Flêche du haut
				if Current_Selection > Menu_Item'First then
					Current_Selection := Menu_Item'Pred(Current_Selection);
				end if;
			elsif Key = 'B' then -- Flêche du bas
				if Current_Selection < Menu_Item'Last then
					Current_Selection := Menu_Item'Succ(Current_Selection);
				end if;
			elsif Key = LF then -- Entrée
				exit;
			end if;
		end loop;

		return Current_Selection;
	end Wait_For_Input;
end Menu;