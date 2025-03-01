with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Menu is
	type Menu_Item is (New_Game, Load_Game, Close);
	Current_Selection : Menu_Item := New_Game;
	Menu_Label : array(Menu_Item) of Unbounded_String := (To_Unbounded_String("Commencer une nouvelle partie"),
												To_Unbounded_String("Charger une partie en cours"),
												To_Unbounded_String("Fermer le jeu")
												);

	-- Effacer l'écran et afficher le menu
	procedure Display_Menu;
	
	-- Renvoie l'option choisie (bloque le processus)
	function Wait_For_Input return Menu_Item;
end Menu;