
load('styles.rdata')

#f$ingredients[240]

malts <- sort(c(
  "Cara-pils",
  "light crystal",
  'Rice',
  "American pale",
  "light dry",
  "light",
  "British pale",
  "extra rich crystal",
  "Klages",
  "Vienna",
  "toasted",
  "lager",
  "crystal",
  "6-row",
  'Pils',
  "2-row",
  "Munich",
  "Pale",
  "toasted pale",
  "chocolate",
  "extra light",
  "amber",
  "dark",
  "Pilsner",
  "aromatic munich",
  "Biscuit",
  "pearl barley",  
  "Blue Ribbon",
  'roast barley',
  'Wheat',
  'Oats',
  #'Barley',
  "flaked Maize"))

hops <- sort(c("Hallertauer",
               'Perle',
                "Eroica",
               'Styrian Goldings',
                "Willamette",
                "Tettnanger",
                "Saaz",
                "Bullion",
                "Cascade",
                "Brewers Gold",
                "Fuggles",
               "Chinook",
               'Centennial',
               'Columbus',
               'Magnum ',
               'Amarillo Gold',
               'Simcoe',
               "Northern Brewer",
                "Goldings"))

cols <- c('#FFE699',
          '#FFD878',
          '#FFCA5A', 
          '#FFBF42',
          '#FBB123', 
          '#F8A600',  
          '#F39C00',#7 
          '#EA8F00',#8 
          '#E58500',#9 
          '#DE7C00',#10 
          '#D77200',#11 
          '#CF6900',#12 
          '#CB6200',#13 
          '#C35900',#14 
          '#BB5100',#15 
          '#B54C00',#16 
          '#B04500',#17
          '#A63E00',#18
          '#A13700',#19
          '#9B3200',#20
          '#952D00',#21
          '#8E2900',#22
          '#882300',#23
          '#821E00',#24
          '#7B1A00',#25
          '#771900',#26
          '#701400',#27
          '#6A0E00',#28
          '#660D00',#29
          '#5E0B00',#30
          '#5A0A02',#31
          '#600903',#32
          '#520907',#33
          '#4C0505',#34
          '#470606',#35
          '#440607',#36
          '#3F0708',#37
          '#3B0607',#38
          '#3A070B',#39
          '#36080A')


