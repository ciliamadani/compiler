%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "BENKACIR_MADANI.tab.h"

extern YYSTYPE yylval;

int InsererTS(char *entitee,char* champ,char *type);
int RechercherTS(char *entitee);

   int ligne=1;
   int col=0;	

typedef struct {char nom[20];char champ[20]; char type[20];} TypeTS;
TypeTS TS[100];
int TailleTS = 0;

%}


idf [A-Z][a-zA-Z0-9]*
cste_entier [+-]?[0-9]+
cste_reel [0-9]+"."[0-9]+
cste_char ('[a-zA-Z]')
cste_string (\"[a-zA-Z]*\")
texte ([A-Za-z0-9][a-zA-Z0-9]+)
texte1   (//[.]*//)                
cste_signe (#|$|&|%) 
%x COMMENT VERIFY

%%
"PROGRAM"     {col=col+yyleng;	return	key_program;}  
"BEGIN"       {col=col+yyleng;	return	key_begin;}  
"END" 	      {col=col+yyleng;	return	key_end;}  
"var"	      {col=col+yyleng;	return	key_var;}  
"INTEGER"     {yylval.chaine=strdup(yytext); col=col+yyleng;	return	key_integer;}  
"FLOAT"	      {yylval.chaine=strdup(yytext); col=col+yyleng;	return	key_float;}  
"CHAR"	      {yylval.chaine=strdup(yytext); col=col+yyleng;	return	key_char;}  
"STRING"      {yylval.chaine=strdup(yytext); col=col+yyleng;	return	key_string;} 
"let"	      {col=col+yyleng;	return	key_let;}  
"RETURN"      {col=col+yyleng;	return	key_return;}  
"IF"	      {col=col+yyleng;	return	key_if;}  
"ELSE"	      {col=col+yyleng;	return	key_else;}  
"END_IF"      {col=col+yyleng;	return	key_end_if;}  
"FOR"	      {col=col+yyleng;	return	key_for;}  
"END_FOR"     {col=col+yyleng;	return	key_end_for;}  
"SHOW"	      {col=col+yyleng;	return	key_show;}  
"GET"	      {col=col+yyleng;	return	key_get;}  
"+"	      {col=col+yyleng;	return	key_addition;}  
"-"	      {col=col+yyleng;	return	key_soustraction;}  
":"	      {col=col+yyleng;	return	key_deux_points;}  
"="	      {col=col+yyleng;	return	key_affectation;}  
"."	      {col=col+yyleng;	return 	key_point;}  
"*"	      {col=col+yyleng;	return	key_multiplication;}  
"/"	      {col=col+yyleng;	return	key_division;}  
"//"	      {col=col+yyleng;	BEGIN(COMMENT);}  
">>"	      {col=col+yyleng;	return	key_superieur;}  
"<<"	      {col=col+yyleng;	return	key_inferieur;}	  
"=="	      {col=col+yyleng;	return	key_egal;} 
">>="	      {col=col+yyleng;	return	key_supegal;}	  
"<<="	      {col=col+yyleng;	return	key_infegal;}  
"<<>>"	      {col=col+yyleng;	return	key_different;}  
"£"	      {col=col+yyleng;	return	key_fin;}  
"||"	      {col=col+yyleng;	return	key_pipe;}  
"("	      {col=col+yyleng;	return	key_parouv;}  
")"	      {col=col+yyleng;	return	key_parfer;}  
"'"	      {col=col+yyleng;	return	key_apostrophe;}  
" " 	      {col=col+yyleng;}  
"\""	      {col=col+yyleng;	return	key_guillemets;} 
"@" 	      {col=col+yyleng;	return	key_signe;} 
"{"	      {col=col+yyleng;	return	key_accouv;}  
"}"	      {col=col+yyleng;	return	key_accfer;}  
"["	      {col=col+yyleng;	return	key_croouv;} 
"]"	      {col=col+yyleng;	return	key_crofer;}  		 
"\n"          {ligne++;col=0;}	  
"\t"	      {col=col+8;}  	
{idf}         {col=col+yyleng;} { yylval.chaine=strdup(yytext); if(yyleng>7){printf("Erreur lexicale! , Ligne: %d ,Col: %d, Entite: %s\n",ligne,col,yytext);} else if (RechercherTS(yytext)==0) InsererTS(yytext,"Idf",""); return idf;}
{cste_entier} {col=col+yyleng;}	{ yylval.entier=atoi(yytext);if(atoi(yytext)>32768 || atoi(yytext)>+32768 || atoi(yytext)<-32768) printf("Erreur lexicale!,Ligne: %d, Col: %d, Entite: %s\n",ligne,col,yytext); else if  (RechercherTS(yytext)==0) {InsererTS(yytext,"Cste","INTEGER");}return cste_entier;} 
{cste_reel}   {col=col+yyleng;} { yylval.reel=atof(yytext); if (RechercherTS(yytext)==0){InsererTS(yytext,"Cste","FLOAT");} return cste_reel;}
{cste_char}   {col=col+yyleng; strcpy(yylval.chaine,strdup(yytext));  if (RechercherTS(yytext)==0) {InsererTS(yytext,"Cste","CHAR");} return cste_char;}
{cste_string} {col=col+yyleng; strcpy(yylval.chaine,strdup(yytext)); if (RechercherTS(yytext)==0) 
{InsererTS(yytext,"Cste","STRING");} 
return cste_string;}
{cste_signe}  { yylval.chaine=strdup(yytext); return cste_signe;}
{texte}	      { return texte;}
.	      {col=col+yyleng;} { printf("Erreur lexicale!, Ligne: %d, Col: %d, Entite:  %s\n",ligne,col,yytext); return key_x;}

<COMMENT>"/"		{ BEGIN(VERIFY);	}
<COMMENT>"\n"		{ ligne++;		        }
<COMMENT>[^"/""\n"]	{					}
<COMMENT>["\""]		{					}
<VERIFY>"/"		{ BEGIN(INITIAL);	}
<VERIFY>[^"/""\n"]	{ BEGIN(COMMENT);	}
<VERIFY>"\n"		{ ligne++; BEGIN(COMMENT); }
<VERIFY>["\""]		{					}

%%


int InsererTS(char *entitee,char* champ,char *type)
{ 
	strcpy(TS[TailleTS].nom, entitee);
	strcpy(TS[TailleTS].type, type);
	strcpy(TS[TailleTS].champ,champ);
	TailleTS++;
 return 0;
}

int RechercherTS(char *entitee)
{ 
	int i;
	for(i=0;i<TailleTS; i++) {if (!strcmp(TS[i].nom, entitee)) return 1;}
	return 0;
}

