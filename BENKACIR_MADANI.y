%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
extern FILE* yyin;

extern int ligne;
extern int col;

typedef struct {char nom[20];char champ[20];char type[20];}TypeTS;

extern TypeTS TS[100];
extern int TailleTS;


int yylex();
int InsererTS(char *entitee, char *type);
int RechercherTS(char *entitee);
int yyerror();
void sauv_type(char id[20],char type[20]);
void CompType_ES(char id[20],char id2[20]);
void CompType_Dec(char id[20],char id2[20]);
int NonDec(char id[20]);
int DoubleDec(char id[20]);
char sauv_idf[20];
char sauv_t[20];
char sauv_cste[20];
char idfs[20][20];
int j=0;
int i;



%}


%union{
char *chaine;
char car;
int entier;
float reel;
}

%token key_program	
%token key_begin	
%token key_end	
%token key_var	
%token <chaine> key_integer	
%token <chaine> key_float	
%token <chaine> key_char	
%token <chaine> key_string	
%token key_const	
%token key_let	
%token key_return	
%token key_if	
%token key_else	
%token key_end_if	
%token key_for	
%token key_end_for	
%token key_show	
%token key_get	
%token key_kaddition	
%token key_soustraction	
%token key_deux_points	
%token key_affectation	
%token key_point	
%token key_multiplication		
%token key_division	
%token key_commentaire	
%token key_superieur	
%token key_inferieur	
%token key_egal	
%token key_supegal	
%token key_infegal	
%token key_different	
%token key_fin	
%token key_pipe	
%token key_parouv	
%token key_parfer	
%token key_apostrophe		
%token key_guillemets	
%token key_signe
%token key_accouv	
%token key_accfer	
%token key_croouv
%token key_crofer	
%token  <chaine> cste_signe
%token	<chaine> idf
%token	<entier> cste_entier
%token	<car>	cste_char
%token	<reel>	cste_reel
%token	<chaine> cste_string
%token <chaine> texte
%token <chaine> texte1
%token key_x
%type <chaine> Type

%type <chaine> Liste_variable
 		
%left key_addition key_soustraction
%left key_multiplication key_division
%left key_superieur key_supegal key_egal key_different key_infegal key_inferieur

%start S
%%

S 					: key_program  idf {sauv_type($2,"mot cle");} Bloc_declaration   key_begin   Bloc_inst  key_end
						;

Bloc_declaration			: key_var Liste_variable Decl1 {if(DoubleDec($2)==1){printf("Erreur Semantique, Double declaration Ligne: %d, Col: %d, Entite: %s\n",ligne,col,$2);} else{for(int i=0;i<=j;i++){sauv_type(idfs[i],sauv_t);} j=0;}} key_fin  Bloc_declaration  
						| key_let idf Decl2 { strcpy(sauv_idf,$2); if(DoubleDec($2)==1){printf("Erreur Semantique, Double declaration Ligne: %d, Col: %d, Entite: %s\n",ligne,col,$2);} else{sauv_type($2,sauv_t);} CompType_Dec(sauv_idf,sauv_cste);}  key_fin  Bloc_declaration 
						|
						;

Decl1					: key_deux_points Type
						| key_croouv Cste key_crofer key_deux_points key_croouv Type key_crofer
						;
						
Decl2					: key_deux_points Type key_affectation Cste 
						| key_affectation Cste 
						;

Liste_variable				: idf {strcpy(idfs[j],$1); j++; } key_pipe Liste_variable  
						| idf {strcpy(idfs[j],$1); j++; }
					;
	
Type					: key_integer {strcpy(sauv_t,$1);}
						| key_string {strcpy(sauv_t,$1);}
						| key_char {strcpy(sauv_t,$1);}
						| key_float {strcpy(sauv_t,$1);}
						;
				
Cste					: cste_entier 
						| cste_char 
						| cste_reel 
						| cste_string {strcpy(sauv_cste,$1);}
						;
						
Bloc_inst				: Inst Bloc_inst
						|
						;
						
						
Inst					: Aff
						| ES
						| Exp_Arithm
						| Cond_if
						| Boucle_for
			
						;
											
						
Aff					:  idf key_affectation Exp  key_fin {if(NonDec($1)==1){printf("Erreur semantique, Variable non declaree Ligne: %d , Col: %d, Entite: %s\n",ligne,col,$1);}}
						;
	
Exp 					: Exp_Arithm
						| cste_char 
						| cste_string 
						;

Exp_Arithm				: 	Exp_Arithm key_addition Exp_Arithm 
						| Exp_Arithm key_soustraction Exp_Arithm
						| Exp_Arithm key_multiplication Exp_Arithm 
						| Exp_Arithm key_division Exp_Arithm
						| idf {if(NonDec($1)==1){printf("Erreur semantique, Variable non declaree Ligne: %d , Col: %d, Entite: %s\n",ligne,col,$1);}}
						| cste_reel	
						| cste_entier	
						| key_parouv Exp_Arithm key_parfer
						;

						
ES					: key_get key_parouv key_inferieur cste_signe key_superieur key_deux_points key_signe  idf key_parfer key_fin { CompType_ES($8,$4);} 
						| key_show key_parouv key_guillemets texte cste_signe key_guillemets key_deux_points idf key_parfer key_fin { CompType_ES($8,$5);} 	
	
						;

						
Cond_if       				: key_if key_parouv Cond key_parfer key_accouv Cond_inst key_accfer Bloc_cond key_end_if
						;

Cond					: Exp_Arithm key_egal Exp_Arithm
						| Exp_Arithm key_supegal Exp_Arithm
						| Exp_Arithm key_infegal Exp_Arithm
						| Exp_Arithm key_different Exp_Arithm
						| Exp_Arithm key_superieur Exp_Arithm
						| Exp_Arithm key_inferieur Exp_Arithm
						| Exp_Arithm
						;

					

Bloc_cond				: key_else key_accouv Cond_inst key_accfer
						|
						;
						
Cond_inst				: Bloc_inst key_return key_parouv Res key_parfer key_fin
						;

Res					: Cste
						| idf {if(NonDec($1)==1){printf("Erreur semantique, Variable non declaree Ligne: %d , Col: %d, Entite: %s\n",ligne,col,$1);}}
						;
						
Boucle_for      			: key_for key_parouv idf {if(NonDec($3)==1){printf("Erreur semantique, Variable non declaree Ligne: %d , Col: %d, Entite: %s\n",ligne,col,$3);}} key_deux_points cste_entier key_deux_points  Cond key_parfer For_inst key_end_for
						;
						
For_inst				: Bloc_inst
						;
						
						
%%
int yyerror ()
{
   printf("Erreur syntaxique, Ligne : %d, Colonne: %d\n",ligne,col );
}


/******************* Verifie la Double Declaration *******************/
int DoubleDec(char id[20]){
	
int i=0;	
while(i<TailleTS){
	if(strcmp(TS[i].nom,id)==0)
	{ if(strcmp(TS[i].type,"")!=0){
		return 1;
		}
		return 0;
	}
i++;
}
}

/******************* Mise a Jour de la TS *************************/
void sauv_type(char id[20],char type[20])
{
int i=0;
while(i<TailleTS) {
if(strcmp(TS[i].nom,id)==0)
{ strcpy(TS[i].type,type);}
 
i++;}
}

/******************* Verifie la Non Declaration *************************/
int NonDec(char id[20]){
int i=0;
while (i<TailleTS){
 if(strcmp(TS[i].nom,id)==0)
	{ if(strcmp(TS[i].type,"")==0){
		return 1;}
	}
   i++;
}
}

/******************* Compatibilite des types dans SHOW et GET (signe de formatage et type idf) *************************/
void CompType_ES(char id[20],char id2[20]){
  int i=0;
  while (i<TailleTS){
 	if(strcmp(TS[i].nom,id)==0){
		
		if(strcmp(id2,"$")==0){if(strcmp(TS[i].type,"INTEGER")!=0){printf("Erreur Semantique, Incompatibilite des types Ligne: %d, Col: %d, Entite: %s, %s\n",ligne,col,TS[i].nom,id2);} break;}
		if(strcmp(id2,"%")==0){if(strcmp(TS[i].type,"FLOAT")!=0){printf("Erreur Semantique, Incompatibilite des types Ligne: %d, Col: %d, Entite: %s, %s\n",ligne,col,TS[i].nom,id2); } break;}
		if(strcmp(id2,"#")==0){if(strcmp(TS[i].type,"STRING")!=0){printf("Erreur Semantique, Incompatibilite des types Ligne: %d, Col: %d, Entite: %s, %s\n",ligne,col,TS[i].nom,id2);}break; }
		if(strcmp(id2,"&")==0){if(strcmp(TS[i].type,"CHAR")!=0){printf("Erreur Semantique, Incompatibilite des types Ligne: %d, Col: %d, Entite: %s, %s\n",ligne,col,TS[i].nom,id2);} break;}
	}
  i++;}
}



/******************* Compatibilite des types dans la Declaration *************************/
void CompType_Dec(char id[20],char id2[20]){
int i=0;
char sauv_t[20];
char sauv_t2[20];
int k=0;

	while(i<TailleTS){
		if(strcmp(TS[i].nom,id)==0){
			strcpy(sauv_t,TS[i].type);k++;}else{if(strcmp(TS[i].nom,id2)==0){strcpy(sauv_t2,TS[i].type);k++;}}
	i++;
	if(k==2){
	if(strcmp(sauv_t,sauv_t2)!=0){printf("Erreur Semantique, Incompatibilite des types Ligne: %d, Col: %d, Entite: %s\n",ligne,col,id);break;}
	}
 }
}


int main(){
  
	yyin=fopen("test2.txt","r");
	yyparse();
	printf("\n\n-----------------------------------------\nTS (Nom) :\t TS(Champ) :\t TS(Type) :\n------------------------------------------\n");
	for(int i=0;i<TailleTS; i++) {if(strcmp(TS[i].type,"")!=0){printf("%s\t\t %s\t\t   %s\n", TS[i].nom,TS[i].champ,TS[i].type);}}
}
