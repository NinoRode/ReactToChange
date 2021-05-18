# Konveksna lupina (convex hull) oblaka točk v n dimenzijah – predprocesiranje in procesiranje

[hipoteza: GLEDANO OD ZNOTRAJ JE VSAKA KONSTRUKCIJA KONVEKSNE LUPINE  TRIDIMENZIONALNI PROBLEM]

Čiščenje notranjih točk
Najdeš glavni centroid (centroid vseh točk)
Izračunaš razdalje točk od glavnega centroida
Za vsako dimenzijo najdeš koordinato glavnega centroida ter skrajni vrednosti (največjo in najmanjšo)
največja razdalja med skrajno vrednostjo in koordinato glavnega centroida je pol stranice hiperkocke okoli centroida, v kateri delamo
Oblikuj hiperkocko okoli centroida in jo razdeli na simplekse (trske – slivers) centroid, projekcija glavnega centroida na zunanji ploskvi/kvadratu in 2 (res samo 2?) točki na stranici
na vsaki stranici izberi primerno število (!ugotovi kako!) segmentov; meje segmentov so oglišča trsk
V vsaki trski najdi najbolj oddaljeno točko
ta točka je (kandidat za) oglišče konveksne lupine
Poveži točke med sabo (najdi presečišča s stranicami trsk?) in formiraj politop
Vse točke, ki so strogo znotraj tega politopa, lahko odstraniš

Konveksna lupina v eni trski
1
Projiciraš točke v trski na ravnino pravokotno na povezavo med glavnim centroidom in njemu najbližjo točko v trski
to je (najverjetneje) skrajna točka (ali pa ni na lupini – preveri – zaenkrat delaj kot da je skrajna)
Najdeš 2D konveksno lupino projekcij na ravnini.
V tej 2D konveksni lupini najdeš 3 centroidu najbližje točke in narediš projekcijsko ravnino skozi njih
IN NE VEM, KAKO NAPREJ

2
Najdeš konveksno lupino točk v trski skupaj s centroidom (quickhull)

(Projiciraš točke v prvi (slučajno izbrani – nepomembno) trski na ravnino skozi centroid v trski pravokotno na povezavo med glavnim centroidom in centroidom točk v tej trski)

Združevanje konveksnih lupin trsk (potrebno bo delati v dveh polovicah)
Izbereš konveksno lupino poljubne trske in  konveksno lupino njej nasprotne trske
vsaka bo seme za eno polovico hiperkocke
Za vsako polovico hiperkocke
Dokler je še kakšna konveksna lupina trske, ki še ni pridružena
Izbereš sosednjo lupino
Za vsak par točk, ki je povezan z glavnim centroidom preveri, če sta njegovi točki v skupni konveksni lupini
če ni, odstrani točko, ki ni v konveksni lupini in (pravilno - kako?) poveži njene sosede med sabo
Združi polovici
Za vsak par točk, ki je povezan z glavnim centroidom preveri, če sta njegovi točki v skupni konveksni lupini
če ni, odstrani točko, ki ni v konveksni lupini in (pravilno - kako?) poveži njene sosede med sabo

____________

TEST LOKALNE KONVEKSNOSTI
[cilindrične koordinate]
[n: število točk, d: število dimenzij]
Vzameš del površine potencialne konveksne lupine [največ polovico – vse točke morajo biti na eni strani glavnega centroida] 
Izračunaš oddaljenost točk od glavnega centroida in razdeliš na višino od glavnega centroida in razdaljo od veznice glavnega centroida in centroida točk na izbrani površini: »normale pogleda«
Urediš obe po velikosti
Iz d+1 točk najbolj oddaljenih od normale pogleda izdelaš osnovni simpleks, izračunaj oddaljenost njihovega baricentra od glavnega centroida in jih dodaj v spisek točk na lupini
Dokler ne zmanjka točk
Vzameš naslednjo najbolj oddaljeno točko
Če je točka znotraj aktivnega simpleksa:
Če je točka pod ravnino simpleksa v katerega pade, jo odpiši (je pod lupino)
Drugače dodaj v spisek točk na lupini in
Če je nad ravnino simpleksa v katerega pade, jo vstavi v simpleks <in odstrani najbližjo točko – jo izbriši iz spiska točk na lupini in njen simpleks zbriši iz spiska aktivnih simpleksov, ali naredi nove simplekse? Vse naslednje točke so bližje normali, zato verjetno prvo, seveda pa za kompletnost rabiš nove simplekse>
Če je točka zunaj vseh aktivnih simpleksov:
Če je pod ravnino aktivnega simpleksa izdelaj nov simpleks in ga dodaj na spisek aktivnih simpleksov
Drugače <je ena od točk preloma pod lupino – obrni simplekse (izbriši povezavo med spodnjima točkama in naredi povezavo med zgornjima !pozor premisli za višje dimenzije, ampak verjetno ne: lupina je površina (dvodimenzionalna)!>
Če je pod ravnino samo ena točka (se vsaj dva simpleksa stekata vanjo navzdol), jo odstrani in med sabo poveži točke, ki so bile povezane z njo

___________________________

Phan Thanh An & Le Hong Trang (2013) An efficient convex hull algorithm for finite point sets in 3D based on the Method of Orienting Curves, Optimization: A Journal of Mathematical Programming and Operations Research, 62:7, 975-988, DOI:
10.1080/02331934.2011.623163, http://dx.doi.org/10.1080/02331934.2011.623163, https://www.researchgate.net/publication/254253623_An_efficient_convex_hull_algorithm_for_finite_point_sets_in_3D_based_on_the_Method_of_Orienting_Curves

v dokazovanju uporablja paralelno projekcijo. Bi lahko uporabil projekcijo iz smeri centroida? Kaj pa iz centroida samega?


___________________________
~~PREPROST~~ REKURZIVEN ALGORITEM (sorodnik quickhull – tower building)
[verjetno O(n log(n)) za res veliko točk naredi iterativno verzijo]
[Da se izognemo kombinatorni eksploziji, kot začetek izberemo iz maksimalnih točk simpleks z največjo prostornino]
~~izračunaj centroid~~ točk premakni centroid v izhodišče
Najdi točke s skrajnimi koordinatami in jih daj v spisek (vogalov) konveksne lupine
Najdi simpleks z največjo prostornino (glavni simpleks)
Iz vsake zunanje ploskve glavnega simpleksa naredi simpleks s centroidom
Za vsakega od teh simpleksov (d) uporabi build_floor 
Procedura build_floor {
najdi točke najbolj oddaljene od centroida (~~min in~~ max abs razdalja = max abs koordinata ~~po eno skrajno koordinato~~ za vsako koordinatno os) za vsako kombinacijo predznakov (tu je kombinatorna eksplozija simpleksov 2<sup>d</sup>) [kaj narediti, če se skrajne vrednosti ponavljajo? => vključiš prvo, ostale bodo prišle v izbor kasneje, saj so vogali lupine, še bolje **vključiš najbolj oddaljeno od centroida** - izključiš največ točk]
```če točk ni dovolj za poln simpleks: {
dodaj vse preostale točke v spisek lupine in nehaj
} drugače: {
točke s skrajnimi koordinatami spravi v spisek lupine
točke s skrajnimi koordinatami poveži v simplekse s centroidom [v bistvu izberi smer, v katero delaš]
za vsak simpleks
najdi ravnino (skozi »najboljše« tri točke [verjetno takšne, ki odrežejo najmanj ali največ]), ki odreže nepomembne točke <razmisli, verjetno je dovolj da obravnavaš samo točke znotraj poligona, ki ga opisujejo skrajne točke, ker bi naj druge bile že v notranjosti?>
izberi vse točke, ki so na drugi strani ravnine kot je glavni centroid, ostale izloči kot nepomembne [kaj s točkami na meji? najverjetneje jih lahko izločiš točke na ravnini so skoraj gotovo znotraj simpleksa]
najdi točke, ki so v isto smer, kot hiperploskev simpleksa nasproti centroida
<izračunaj centroid stranice nasproti prejšnjega centroida [verjetno je to nepotreben korak]>
build_floor
}
```
iz točk v spisku lupine izračunaj lupino s ~~quickhull,~~ gift wrap, **divide and conquer** ali kaj podobnega
{-_-}
Stvar ni tako preprosta: naslednji sloj je lahko veliko višje in vidi sosednje facete.

{°_°}
Še zmeraj lahko delaš tako, kot si začel: če na koncu najdeš špico (končna točka ali največ d-1 točk), je vsaj en njen del sestavni del lupine.
Ko imaš vse špice, jih najprej povežeš z dolinami, da se rešiš konkavnih pobočij,
potem jih vežeš med sabo po sistemu vidnosti od vrha navzdol.

__________

Vidnost točke
Točka je vidna, če so vse točke, ki so »vmes,« na eni strani linije pogleda, če je njena projekcija na pravokotno hiperrevanino skrajna (najdi citat). 
