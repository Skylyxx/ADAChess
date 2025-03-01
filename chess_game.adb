with Chess_Board;
use Chess_Board;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Chess_Game is

	-- Renvoie le plateau de jeu
	function Board(Game : in T_Game) return T_Chessboard is
	begin
		return Game.Board;
	end Board;

	-- Renvoie le joueur qui doit jouer
	function Turn(Game : in T_Game) return T_Team is
	begin
		return Game.Turn;
	end Turn;

	-- Renvoie le status de la partie
	function Status(Game : in T_Game) return T_Status is
	begin
		return Game.Status;
	end Status;

	-- Initialiser une partie
	procedure Init_Game(Game : out T_Game) is
		Board : T_Chessboard;
	begin
		Init_Board(Board);
		Game := (Board, White, Playing);
	end Init_Game;

	-- Lancer une partie / jouer le prochain tour
	procedure Next_Turn(Game : in out T_Game ; Highlight : in T_Cases := (others => 0)) is
		MoveInputUnbounded : Unbounded_String;

		function Valid_Input(Input : in String) return Boolean is
		begin
			-- Put_Line(Input(Input'First..Input'First+1));
			-- Put_Line(Input(Input'First+3..Input'First+4));
			if Get_Case_Id(Input(Input'First..Input'First+1)) = 64 or Get_Case_Id(Input(Input'First+3..Input'First+4)) = 64 then
				return False;
			end if;

			return True;
		end Valid_Input;
	begin
		for I in 0..10 loop
			New_Line;
		end loop;
		Display_Board(Board => Game.Board, Reversed => Game.Turn = Black, Highlight => Highlight);

		-- Mise à jour du status de la partie
		-- if Is_Check(Game.Board, White) then
		-- 	if Legal_Moves_Count(Game.Board, White) = 0 then
		-- 		Game.Status := Checkmate_White;
		-- 	else
		-- 		Game.Status := Check_White;
		-- 	end if;
		-- elsif Is_Check(Game.Board, Black) then
		-- 	if Legal_Moves_Count(Game.Board, Black) = 0 then
		-- 		Game.Status := Checkmate_Black;
		-- 	else
		-- 		Game.Status := Check_Black;
		-- 	end if;
		-- else
		-- 	if Legal_Moves_Count(Game.Board, Black) = 0 or Legal_Moves_Count(Game.Board, White) = 0 then
		-- 		Game.Status := Draw;
		-- 	else
		-- 		Game.Status := Playing;
		-- 	end if;
		-- end if;

		case Game.Status is
			when Checkmate_White =>
				New_Line;
				New_Line;
				Put_Line(ESC & "[44m Victoire des Noirs par échec et mat !" & ESC & "[0m");
				New_Line;
				New_Line;
				return;
			when Checkmate_Black =>
				New_Line;
				New_Line;
				Put_Line(ESC & "[44m Victoire des Blancs par échec et mat !" & ESC & "[0m");
				New_Line;
				New_Line;
				return;
			when Draw =>
				New_Line;
				New_Line;
				Put_Line(ESC & "[44m Match nul (aucun mouvement légal disponible pour une des équipes)" & ESC & "[0m");
				New_Line;
				New_Line;
				return;
			when others => null;
		end case;


		MoveInputUnbounded := Ask_For_Input(Game);
		declare
			MoveInput : String := To_String(MoveInputUnbounded);
			From,Dest : Natural;
			NewHighlight : T_Cases := (others => 0);
			Moves : T_Cases;
		begin

			if MoveInput'Length < 3 then
				Next_Turn(Game); -- on redemande
			else
				-- Cas 1: ?e2 (on montre les cases disponibles depuis e2)
				if MoveInput(MoveInput'First) = '?' then
					From := Get_Case_Id(MoveInput(MoveInput'First+1..MoveInput'First+2));

					-- Case valide ?
					if From <= 63 then
						NewHighlight(From) := 4; -- 4: bleu
						
						Moves := Legal_Moves(Game.Board, Game.Turn, From);
						for I in Moves'Range loop
							if Moves(I) = 1 then 
								NewHighlight(I) := 5; -- 5: violet
							end if;
						end loop;
					end if;

					Next_Turn(Game, NewHighlight);

				-- Cas 2: !e2=e4 (on fait le move meme si illégal)
				elsif Valid_Input(MoveInput(MoveInput'First..MoveInput'Last)) or else (MoveInput(MoveInput'First) = '!' and Valid_Input(MoveInput(MoveInput'First+1..MoveInput'Last))) then
					if MoveInput(MoveInput'First) = '!' then
						From := Get_Case_Id(MoveInput(MoveInput'First+1..MoveInput'First+2));
						Dest := Get_Case_Id(MoveInput(MoveInput'First+4..MoveInput'First+5));
					else
						From := Get_Case_Id(MoveInput(MoveInput'First..MoveInput'First+1));
						Dest := Get_Case_Id(MoveInput(MoveInput'First+3..MoveInput'First+4));
					end if;

					if Dest /= From and (MoveInput(MoveInput'First) = '!' or Is_Legal_Move(Board => Game.Board, Turn => Game.Turn, From => From, Dest => Dest)) then
						Game.Board(Dest) := Game.Board(From);
						Game.Board(From) := Empty_Piece;

						Game.Turn := (if Game.Turn = White then Black else White);
					end if;

					Next_Turn(Game);
				else
					Next_Turn(Game); -- on redemande
				end if;
			end if;
		end;
	end Next_Turn;

	function Ask_For_Input(Game : in T_Game) return Unbounded_String is
		Input_Line : String(1..10);
		Last : Natural;
	begin
		New_Line;
		Put_Line("C'est au tour des: " & Translate_Team(Game.Turn));
		Put_Line(ESC & "[3;36mPour jouer un coup, entrez '<from>=<to>'. Exemple: e2=e4" & ESC & "[0m");
		Put_Line(ESC & "[3;36mPour voir les coups légaux d'une pièce, entrez '?<from>'. Exemple: ?b1" & ESC & "[0m");
		Put(ESC & "[31m>> " & ESC & "[0m");

		return To_Unbounded_String(Get_Line);
	end Ask_For_Input;

end Chess_Game;