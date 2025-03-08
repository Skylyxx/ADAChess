with Chess_Board;
use Chess_Board;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Chess_Game is
	
	type T_Game is private;
	type T_Status is (Playing, Check_White, Check_Black, Checkmate_White, Checkmate_Black, Draw);

	-- Renvoie le plateau de jeu
	function Board(Game : in T_Game) return T_Chessboard;
	-- Renvoie le joueur qui doit jouer
	function Turn(Game : in T_Game) return T_Team;
	-- Renvoie le status de la partie
	function Status(Game : in T_Game) return T_Status;

	-- Initialiser une partie
	procedure Init_Game(Game : out T_Game);
	-- Charger une partie: renvoie vrai ssi la dispoition est valide
	function Load_Game(Game : out T_Game) return Boolean;
	-- Obtenir la notation FEN de la partie courrante
	function Get_Current_Fen(Game : in T_Game) return String;

	-- Lancer une partie / jouer le prochain tour
	procedure Next_Turn(Game : in out T_Game ; Highlight : in T_Cases := (others => 0));

private

	type T_Game is record
		Board : T_Chessboard;
		Turn : T_Team;
		Status : T_Status;
	end record;

end Chess_Game;