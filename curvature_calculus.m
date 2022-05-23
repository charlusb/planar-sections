fid = fopen('courbes040421.txt','a');


%%geodesique cone
surface = "cone";
inters = "NI";
geod = "D";
nom = "ruban geodesique";


r = 2.5;
h = 6.25;
a =  r/sqrt(r^2+h^2);
k = r/h;




%%% intervalle de t
tmin =  0.39 ;
tmax =  7.47 ;
t = tmin:((tmax-tmin)/1000):tmax;
t = t(2:1000);

%%%% formules de la courbe
%%%  Ici un cercle sur un plan horizontal et de rayon a=1

x = k*14*cos(t).*cos(a*pi / 4 + a*pi) ./ cos(a*t - a*pi / 4 - a*pi) ;
y = k*14*sin(t).*cos(a*pi/ 4 + a*pi) ./ cos(a*t - a*pi / 4 - a*pi) ;
z = 14*cos(a*pi / 4 + a*pi) ./ cos(a*t - a*pi / 4 - a*pi) ;

  
interval = find(z<6.25);
x=x(interval);
y=y(interval);
z=z(interval);

%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);

long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);

x1(end) = [];
y1(end) = [];
z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);



cmin
cmax
Cmean
fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

%%ruban cone d�viante

surface = "cone";
inters = "NI";
geod = "ND";
nom = "ruban deviant";

r = 2.5;
h = 6.25;
a =  r/sqrt(r^2+h^2);
k = r/h;
%%% intervalle de t
tmin = -0.53 ;
tmax =  7.86 ;
t = tmin:((tmax-tmin)/1000):tmax;


x = k*cos(t).*log(30*cos(7*a*pi/ 6) ./ cos(a*t - 7*a*pi / 6)) ;
y = k*sin(t).*log(30*cos(7*a*pi/ 6) ./ cos(a*t - 7*a*pi/ 6)) ;
z = log(30*cos(7*a*pi/ 6) ./ cos(a*t - 7*a*pi/ 6)) ;

interval = find(z<6.25);
x=x(interval);
y=y(interval);
z=z(interval);
%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);

%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);
%% dans le cas d'une courbe ferm�e
%x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
%y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
%z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);
%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);
x1(end) = [];
y1(end) = [];
z1(end) = [];


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);
normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);
C = normproduit./(norme_f1.^3);
cmin = min(C);
cmax = max(C);
Cmean = mean(C);

cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)


%% cone tranche tordue

surface = "cone";
inters = "NI";
geod = "ND";
nom = "tranche tordue";

r = 2.5;
h = 6.25;
a =  r/sqrt(r.^2+h.^2);
k = r/h;
%%% intervalle de t
tmin = -1.85 ;
tmax =  1.85 ;
t = tmin:((tmax-tmin)/1000):tmax;
x = t ;
y = t.^4 / 10 + 0.5;
z = 6.25 - sqrt((t.^2 + (t.^4 / 10 + 0.5).^2) / k.^2) ;

interval = find(z<6.25);
x=x(interval);
y=y(interval);
z=z(interval);

%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);


%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

x1(end) = [];
y1(end) = [];
z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

 
cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)
%%Cone Tranche droite 


surface = "cone";
inters = "I";
geod = "ND";
nom = "tranche droite";

r = 2.5;
h = 6.25;
a =  r/sqrt(r.^2+h.^2);
k = r/h;
%%% intervalle de t
tmin = -2.45 ;
tmax =  2.45 ;
t = tmin:((tmax-tmin)/1000):tmax;

x = t ;
y = (0.5+t)-t ;
z = 6.25 - sqrt((0.5.^2 + t.^2) / k.^2) ;

interval = find(z<6.25);
x=x(interval);
y=y(interval);
z=z(interval);

%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);


%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

x1(end) = [];
y1(end) = [];
z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)



%% cone CI

surface = "cone";
inters = "I";
geod = "ND";
nom = "cercle";

r = 1.2;

%%% intervalle de t
tmin = 0 ;
tmax =  2*pi ;
t = tmin:((tmax-tmin)/1000):tmax;
x = r*cos(t) ;
y = r*sin(t);
z = zeros(size(x)); 


%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);



%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%% dans le cas d'une courbe ferm�e
x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));


norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

 
cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

     
%%Cylindre ellipse d�viante

surface = "cylindre";
inters = "NI";
geod = "ND";
nom = "ellipse deviante";

r = 2;




%%% intervalle de t: dans le cas d'une courbe ferm�e, le tmin et tmax
%%% doivent d�signer le m�me point sur la courbe
tmin = -2*pi;
tmax = 2*pi;

t = tmin:((tmax-tmin)/1000):tmax;

%%%% formules de la courbe
%%%  Ici un cercle sur un plan horizontal et de rayon a=1



x = 2*cos(t);
y = 2*sin(t);
z = (sin(t) + 1).^ 2 / 2 + 1.5;

%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);


%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);

%% dans le cas d'une courbe ferm�e
x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

%%% pour le cas d'une courbe ouverte
% x2 = diff(x1/((tmax-tmin)/1000);
% y2 = diff(x2/((tmax-tmin)/1000);
% z2 = diff(x3/((tmax-tmin)/1000);
% x1(end) = [];
% y1(end) = [];
% z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);
normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C)
cmax = max(C)
Cmean = mean(C)

cmin
cmax

Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

%%Cylindre ellipse d�viante

surface = "cylindre";
inters = "I";
geod = "ND";
nom = "ellipse";

r = 2;




%%% intervalle de t: dans le cas d'une courbe ferm�e, le tmin et tmax
%%% doivent d�signer le m�me point sur la courbe
tmin = -2*pi;
tmax = 2*pi;

t = tmin:((tmax-tmin)/1000):tmax;

%%%% formules de la courbe
%%%  Ici un cercle sur un plan horizontal et de rayon a=1



x = 2*cos(t);
y = 2*sin(t);
z = sin(t) + 2;

%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);


%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);

%% dans le cas d'une courbe ferm�e
x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

%%% pour le cas d'une courbe ouverte
% x2 = diff(x1/((tmax-tmin)/1000);
% y2 = diff(x2/((tmax-tmin)/1000);
% z2 = diff(x3/((tmax-tmin)/1000);
% x1(end) = [];
% y1(end) = [];
% z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);
normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C)
cmax = max(C)
Cmean = mean(C)


cmin
cmax

Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)


%%cylindre spirale geodesique large
surface = "cylindre";
inters = "NI";
geod = "D";
nom = "spirale geodesique large";





%%% intervalle de t
tmin = 0;
tmax = 2*pi-0.98;
t = tmin:((tmax-tmin)/1000):tmax;

%%%% formules de la courbe
%%%  Ici un cercle sur un plan horizontal et de rayon a=1



x = 2*cos(t) ;
y = 2*sin(t) ;
z = t ;

interval = find(z<5.35);
x=x(interval);
y=y(interval);
z=z(interval);

plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);


%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

x1(end) = [];
y1(end) = [];
z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

 
cmin
cmax
Cmean

	    fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

%%cylindre spirale d�viante 1 tours

surface = "cylindre";
inters = "NI";
geod = "ND";
nom = "spirale d�viante large tours";




%%% intervalle de t
tmin = 0;
tmax = 2*pi-0.56;
t = tmin:((tmax-tmin)/1000):tmax;

%%%% formules de la courbe
%%%  Ici un cercle sur un plan horizontal et de rayon a=1



x = 2*cos(t) ;
y = 2*sin(t) ;
z = sqrt(5*t) ;

interval = find(z<5.35);
x=x(interval);
y=y(interval);
z=z(interval);



%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);


%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

x1(end) = [];
y1(end) = [];
z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

 
cmin
cmax
Cmean

     fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)


%%cylindre spirale geodesique large courbure
surface = "cylindre";
inters = "NI";
geod = "PD";
nom = "spirale large d�viante courbure";





%%% intervalle de t
tmin = 0;
tmax = 2*pi-2.64;
t = tmin:((tmax-tmin)/1000):tmax;


x = 2*cos(t) ;
y = 2*sin(t) ;
z = sqrt(7.75*t) ;

interval = find(z<5.35);
x=x(interval);
y=y(interval);
z=z(interval);

%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);


%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

x1(end) = [];
y1(end) = [];
z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

     
cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

%%cylindre spirale geodesique �troite

surface = "cylindre";
inters = "NI";
geod = "D";
nom = "spirale etroite";



%%% intervalle de t
tmin = 0.15;
tmax = 21.33;
t = tmin:((tmax-tmin)/1000):tmax;



x = 2*cos(t) ;
y = 2*sin(t) ;
z = t/4 ;

interval = find(z<5.35);
x=x(interval);
y=y(interval);
z=z(interval);

plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);


%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

x1(end) = [];
y1(end) = [];
z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

     
cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

%%cylindre spirale d�viante �troite

surface = "cylindre";
inters = "NI";
geod = "ND";
nom = "spirale deviante etroite tours";


%%% intervalle de t
tmin = 0;
tmax = 22.6;
t = tmin:((tmax-tmin)/1000):tmax;

%%%% formules de la courbe
%%%  Ici un cercle sur un plan horizontal et de rayon a=1



x = 2*cos(t) ;
y = 2*sin(t) ;
z = sqrt(t / 0.8) ;

interval = find(z<5.35);
x=x(interval);
y=y(interval);
z=z(interval);

%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut

plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);

%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

x1(end) = [];
y1(end) = [];
z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

     
cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

%%cylindre spirale d�viante �troite

surface = "cylindre";
inters = "NI";
geod = "ND";
nom = "spirale deviante etroite courbure";


%%% intervalle de t
tmin = 0;
tmax = 5.5;
t = tmin:((tmax-tmin)/1000):tmax;

%%%% formules de la courbe
%%%  Ici un cercle sur un plan horizontal et de rayon a=1



x = 2*cos(t) ;
y = 2*sin(t) ;
z = sqrt(t * 5.2);

interval = find(z<5.35);
x=x(interval);
y=y(interval);
z=z(interval);

%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut

plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);

%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

x1(end) = [];
y1(end) = [];
z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

     
cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

%%cercle d�viant sph�re

surface = "sphere";
inters = "NI";
geod = "ND";
nom = "cacahouete";


%%% intervalle de t
tmin = 0;
tmax = 2*pi;
t = tmin:((tmax-tmin)/1000):tmax;

%%%% formules de la courbe
%%%  Ici un cercle sur un plan horizontal et de rayon a=1


x=2.5*sin(sin(2*t) / 6 + 7*pi / 22).*cos(t);
y=2.5*sin(sin(2*t) / 6 + 7*pi / 22).* sin(t);
z=2.5*cos(sin(2*t) / 6 + 7*pi / 22);


%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);

long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%%% pour le cas d'une courbe ouverte
x2 = diff(x1)/((tmax-tmin)/1000);
y2 = diff(y1)/((tmax-tmin)/1000);
z2 = diff(z1)/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));

x1(end) = [];
y1(end) = [];
z1(end) = [];

norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

     
cmin
cmax
Cmean


fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

%% sph�re CI

surface = "sph�re";
inters = "I";
geod = "PD";
nom = "petit_cercle";

r = 2.5;

%%% intervalle de t
tmin = 0 ;
tmax = 2*pi ;
t = tmin:((tmax-tmin)/1000):tmax;

x = 2.5*sin(pi / 4)*cos(t) ;
y = 2.5*sin(pi / 4)*sin(t);
z = zeros(size(x)); 


%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);



%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%% dans le cas d'une courbe ferm�e
x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));


norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

     
 
cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)


%% sphere DI

surface = "sphere";
inters = "I";
geod = "D";
nom = "grand cercle";

r = 2.5;

%%% intervalle de t
tmin = 0 ;
tmax =  2*pi ;
t = tmin:((tmax-tmin)/1000):tmax;
x = r*cos(t) ;
y = r*sin(t);
z = zeros(size(x)); 


%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);



%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%% dans le cas d'une courbe ferm�e
x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));


norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

 
cmin
cmax
Cmean


fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

%%%cylindre grand DI

surface = "cylindre";
inters = "I";
geod = "D";
nom = "grand cercle";

r = 2;

%%% intervalle de t
tmin = -2*pi ;
tmax =  2*pi ;
t = tmin:((tmax-tmin)/1000):tmax;
x = r*cos(t) ;
y = r*sin(t);
z = zeros(size(x)); 


%%% faire une figure avec la courbe pour �tre s�r que c'est bon
%%% (ie la forme est bonne et aussi elle est bien homog�ne, pas de saut
plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);



%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%% dans le cas d'une courbe ferm�e
x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));


norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);


cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)



%%%%%%% tore top circle
surface = "tore";
inters = "I";
geod = "C";
nom = "top circle";

r1 = 2;
r2 = 0.5;

%%% intervalle de t
tmin = 0 ;
tmax =  2*pi ;
t = tmin:((tmax-tmin)/1000):tmax;
x = r1*cos(t) ;
y = r1*sin(t);
z = zeros(size(x)); 


plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);



%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%% dans le cas d'une courbe ferm�e
x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));


norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

     
cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)


%%%%%%% tore extremal circle
surface = "tore";
inters = "I";
geod = "D";
nom = "extremal circle";

r1 = 2;
r2 = 0.5;

%%% intervalle de t
tmin = 0 ;
tmax = 2*pi ;
t = tmin:((tmax-tmin)/1000):tmax;

x = (r1+r2)*cos(t) ;
y = (r1+r2)*sin(t);
z = zeros(size(x)); 


plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);



%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%% dans le cas d'une courbe ferm�e
x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));


norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);


cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)

%%%%%%% tore central circle
surface = "tore";
inters = "I";
geod = "D";
nom = "central circle";

r1 = 2;
r2 = 0.5;

%%% intervalle de t
tmin = 0 ;
tmax = 2*pi ;
t = tmin:((tmax-tmin)/1000):tmax;

x = (r2-r1)*cos(t) ;
y = (r2-r1)*sin(t);
z = zeros(size(x)); 


plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);



%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%% dans le cas d'une courbe ferm�e
x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));


norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

     
cmin
cmax
Cmean

     fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)






%%%%%%% tore cacahouete
surface = "tore";
inters = "NI";
geod = "C";
nom = "cacahouète";

r1 = 2;
r2 = 0.5;

%%% intervalle de t
tmin = 0 ;
tmax = 2*pi ;
t = tmin:((tmax-tmin)/1000):tmax;

x=2.7*sin(sin(2*t) / 10 + 7*pi / 22).*cos(t);
y=2.7*sin(sin(2*t) / 10 + 7*pi / 22).* sin(t);
z=cos(sin(2*t) / 10 + 7*pi / 22) - 0.1;


plot3(x,y,z); axis equal
fr=getframe();
im = frame2im(fr);
imwrite(im,sprintf('%s_%s_%s_%s.tiff',nom,surface,inters,geod),'Resolution',360);



%%% calculer les diff�rencielles
x1 = diff(x)/((tmax-tmin)/1000);
y1 = diff(y)/((tmax-tmin)/1000);
z1 = diff(z)/((tmax-tmin)/1000);


%% dans le cas d'une courbe ferm�e
x2 = diff([x1 x1(1)])/((tmax-tmin)/1000);
y2 = diff([y1 y1(1)])/((tmax-tmin)/1000);
z2 = diff([z1 z1(1)])/((tmax-tmin)/1000);


long = sum(sqrt(diff(x).^2+diff(y).^2+diff(z).^2));


norme_f1 = sqrt(x1.^2+y1.^2+z1.^2);
produit = cross([x1;y1;z1],[x2;y2;z2]);

normproduit = sqrt(produit(1,:).^2+produit(2,:).^2+produit(3,:).^2);

C = normproduit./(norme_f1.^3);

cmin = min(C);
cmax = max(C);
Cmean = mean(C);

     
cmin
cmax
Cmean

fprintf(fid,'%s %s %s %s %.2f %.4f %.4f %.4f\n',nom,surface,inters,geod,long,cmin,cmax,Cmean)


fclose(fid);


