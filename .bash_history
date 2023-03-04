cd /etc/udev/rules.d/
sudo apt install xbacklight
xbacklight -set 50
ls /sys/class/backlight/
man xbacklight 
xbacklight --help
ls /etc/X11/
cd /etc/X11/
cd xorg.conf.d/
cd ..
cd
xrandr 
xbacklight --help
xbacklight -d eDP -get
sudo find /sys/ -type f -iname '*brightness*'
xbacklight -d amdgpu_bl1 -get
sudo apt remove  xbacklight 
xrandr --output eDP  --brightness 0.8
xrandr 
xrandr --output XWAYLAND0  --brightness 0.8
xrandr --output XWAYLAND0  --brightness 0.5
vim /etc/default/grub
sudo vim /etc/default/grub
sudo update-grub
sudo vim /etc/default/grub
sudo update-grub
xev
amixer
amixer set Master 50% unmute
man amixer
amixer controls
git clone https://github.com/Ventto/lux.git && cd lux && sudo make install && sudo lux
lux
man lux
lux
lux -G
lux -g
lspci
lsusb
iwconfig 
iwconfig wlp2s0 
ifconfig 
bluez
sudo apt install bluez
man bluez
nm-applet 
man nm-applet 
rfkill
scan on
bluetoothctl show
bluetoothctl
pulseaudio 
cd /etc/pulse/
vim client.conf
vim daemon.conf 
vim default.pa
cd client.conf.d/
vim 01-enable-autospawn.conf 
vim /run/pulseaudio-enable-autospawn
cd ..
vim system.pa 
cd /etc/alsa/conf.d/
vim 99-pulseaudio-default.conf.example 
cd /etc/
cd
cc
pactl
sudo apt install pulseaudio-utils pavucontrol
pactl
amixer
pactl info
pactl
pactl stat
xev
iwconfig 
man iwconfig 
iwconfig 
cd .config/xmonad/
vim layout_switch.sh 
./layout_switch.sh 
bash ./layout_switch.sh 
bash layout_switch.sh 
sudo chmod +x layout_switch.sh 
./layout_switch.sh 
vim layout_switch.sh 
vim layout_switch.sh ru
./layout_switch.sh  ru
./layout_switch.sh  ут
./layout_switch.sh  us
./layout_switch.sh ru
./layout_switch.sh  us
cd
cd lux/
vim lux.sh
cd
whereis lux
man lux
vim .xboardrc 
sudo apt remove xboard
sudo apt autoremove
cc
cd Downloads/
xpdf elegant-objects-by-yegor-bugayenko.pdf 
evince ele
evince elegant-objects-by-yegor-bugayenko.pdf 
setxkbmap -query
man awk
setxkbmap -query | awk "END{print $2}"
setxkbmap -query | awk "END{print $3}"
man awk
setxkbmap -query | awk "print /^[usr]"
setxkbmap -query | mawk "print /^[usr]"
man awk
setxkbmap -query | awk "print /^[usr]"/
setxkbmap -query | awk "print /^[usr]/"
setxkbmap -query | awk "/^[usr]/"
man awk
setxkbmap -query | awk "/^[usr]*$/"
setxkbmap -query
setxkbmap -query | awk "END{print $1}"
setxkbmap -query | awk "END{print $5}"
setxkbmap -query | awk "END{print}"
setxkbmap -query | awk "END{print $2}"
man setxkbmap 
setxkbmap -layout
man setxkbmap 
setxkbmap -query | awk 
setxkbmap -query | awk "END{print}"
setxkbmap -query | awk "layout"
setxkbmap -query | awk "{print}"
setxkbmap -query | awk "{print}" | grep "lay"
setxkbmap -query | awk "{print}" | grep "lay" |awk "END{print}"
setxkbmap -query | awk "{print}" | grep "lay" |awk "END{print $2}"
man grep
setxkbmap -query | cut -d ":" -f 2
setxkbmap -query | grep "layout" | cut -d ":" -f 2
setxkbmap -query | grep "layout" | cut -d " " -f 5
setxkbmap -query | grep "layout" | cut -d " " -f 6
setxkbmap -query | grep "layout" | cut -d " " f- 1
setxkbmap -query | grep "layout" | cut -d " " -f 1-
setxkbmap -query | grep "layout" | cut -d " " -f 2-
setxkbmap -query | grep "layout" | cut -d " " -f -2
setxkbmap -query | grep "layout" | cut -d " " -f 2-
setxkbmap -query | grep "layout" rev  | cut -d " " -f 2-
setxkbmap -query | grep "layout" | rev | cut -d " " -f 2-
setxkbmap -query | grep "layout" | rev | cut -d, -f2
setxkbmap -query | grep "layout" | rev | cut -d " ", -f2
setxkbmap -query | grep "layout" | rev | cut -d " " -f2
setxkbmap -query | grep "layout" | rev | cut -d " " -f1
setxkbmap -query | grep "layout" | rev | cut -d " " -f1 | rev
cd .config/xmonad/
vim layout_switch.sh 
help test
help test |less
fg
layout_switch.sh 
./layout_switch.sh 
vim layout_switch.sh 
./layout_switch.sh 
vim layout_switch.sh 
./layout_switch.sh 
vim layout_switch.sh 
./layout_switch.sh 
cc
setxkbmap -query | awk '{prevlast = last; last = $0} END {if (NR >= 2) print "penultimate:", prevlast}'
setxkbmap -query | awk '{prevlast = last; last = $0} END {if (NR >= 2) print, prevlast}'
setxkbmap -query | awk '{prevlast = last; last = $0} END {if (NR >= 2) print , prevlast}'
setxkbmap -query | awk '{prevlast = last; last = $0} END {if (NR >= 2) print prevlast}'
setxkbmap -query | sed '3'
setxkbmap -query | sed -e '3'
setxkbmap -query | sed 'layout/p'
setxkbmap -query | sed 's/layout/p'
setxkbmap -query | sed '2d'
setxkbmap -query | sed '1,2,4d'
setxkbmap -query | sed '1,2,d'
setxkbmap -query | sed '1d'
setxkbmap -query | sort
setxkbmap -query | sort | awk "BEGIN{print $1}
setxkbmap -query | sort | awk "BEGIN{print $1}"
setxkbmap -query | sort | awk "BEGIN{print $2}"
setxkbmap -query | sort | awk '{print $2}'
setxkbmap -query | awk '{print $2}'
setxkbmap -query | awk 'END {print $2}'
setxkbmap -query | awk 'END{print $2}'
setxkbmap -query | awk '{print $2}'
setxkbmap -query | sort | awk '{print $2}'
setxkbmap -query | sort | awk 'BEGIN {print $2}'
setxkbmap -query | sort | awk 'END {print $2}'
setxkbmap -query | sort -r | awk '{print $2}'
setxkbmap -query | sort -r | awk 'END {print $2}'
setxkbmap -query | sort -r | awk 'END{print $2}'
cd .config/xmonad/
./layout_switch.sh 
man wc
cd .local/bin/
cp check_updates ~/.config/xmonad/
cd ~/.config/xmonad/
mv check_updates check_updates.sh
ls -1
mna ls
man ls
cc
free -h
setxkbmap us
setxkbmap ru
setxkbmap us
which xmonad.desktop
whereis xmonad.desktop
find xmonad.desktop
ll /usr/local/etc/
ll /usr/local/bin/
ll /usr/share/xsessions/
cd projects/dotfiles/
git status
git add .
git commit -m "Added xmonad config"
git push
cd projects/
cd ruby/
cd iri 
cd lib/
emacsclient -n iri.rb 
emacsclient -c iri.rb 
libinput
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd ..
cd dotfiles/
git status 
ga
gadd
git add .
git commit -m "Added stack.yaml to xmobar"
git push
xinput list
xinput list-props 13
sudo xinput set-prop 13 295 1
cd .config/xmobar/
rm -rf .stack-work/
cat stack.yaml
c
cc
stack install xmobar
sudo apt-get install -y xorg-dev libxrandr-dev libpango1.0-dev
sudo   apt-get install -y libasound2-dev libxpm-dev libmpd-dev
rm -rf .stack-work/
stack install
cd
rm -rf .stack/
curl -sSL https://get.haskellstack.org/ | sh
cd .local/bin/
stack uninstall
which stack
sudo rm /usr/local/bin/stack 
curl -sSL https://get.haskellstack.org/ | sh
sudo rm -rf /usr/bin/stack 
curl -sSL https://get.haskellstack.org/ | sh
cd 
cd .config/xmobar/
rm -rf .stack-work/
stack install
шцсщт
iwconfig 
c
cc
iwconfig 
man iwconfig 
iwconfig 
cc
qq
sudo xinput set-prop 13 295 1
xinput list
xinput list-prop 14
xinput list-props 14
sudo xinput set-prop 14 295 1
qq
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
sudo apt install sushi
sudo apt update 
sudo apt upgrade 
sudo apt install gnome-sushi
cd projects/work/applications_table/refactoring/count_inf_tel/
workon xlsx
python df_format.py 
cd projects/work/applications_table/refactoring/count_inf_tel/
workon xlsx
python df_format.py 
ghci
which ghci
which ghc
cd projects/haskell/
stack new real
cd real/
ttree
tree
stack build
tree
cd ..
cd real/
cd .s
cd .stack-work/
cd install/
cd ..
cd dist/
cd x86_64-linux-tinfo6/
cd ..
cc
cd ..
stack exec real-exe
cc
cd
cd .local/bin/
cc
cd 
cd projects/haskell/real/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.6.3.0/build/real-exe/
real-exe
./real-exe
stack install
real-exe
cd ..
cd src/
emacsclient -c Lib.hs 
cd ..
cd app
emacsclient -c Main.hs 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
which ghcup
cc
sudo apt install build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
stack uninstall
cd .local/bin
cd /usr/bin
ll | grep "stack"
cd
cc
whereis stack
cd /usr/local/bin
sudo rm stack 
cd
stack uninstall
rm -rf .stack/
curl -sSL https://get.haskellstack.org/ | sh
sudo apt install haskell-stack 
sudo apt update
sudo atpt upgrade
sudo apt upgrade
whereis stack
apt search  haskell
apt search  haskell | less
apt search  haskell | grep 'server' | less
apt search  haskell | grep 'server' 
q
cc
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
sudo apt remove haskell-stack 
sudo apt autoremove 
cc
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup tui
. ./.bashrc 
ghcup tui
cc
htop
. ./.bashrc 
sudo apt install libicu-dev libtinfo-dev libgmp-dev zlib1g-dev
whereis ghc
whereis ghci
ghci
ghc
htop
ghcup tiu
ghcup tui
sudo apt autoremove 
sudo apt autoclean
cc
whereis stack
whereis cabal
sudo apt remove ghc
cc
curl -sSL https://get.haskellstack.org/ | sh
atack install haskell-language-server
stack install haskell-language-server
cd Downloads/
git clone https://github.com/haskell/haskell-language-server
cd haskell-language-server/
vim README.md 
stack install haskell-language-server
cd ..
cd haskell-language-server/
vim stack.yaml
cd src/
cd ..
vim Setup.hs 
stack install 
cd projects/work/applications_table/refactoring/merge_files_toml/
./tprocessor 
. ./.bashrc
./tprocessor 
emacs --version
htop
stack exec --ghc --version
chmod +x ~/.local/bin/haskell-language-server
chmod +x ~/.local/bin/haskell-language-server-wrapper 
haskell-language-server
haskell-language-server-wrapper 
cc
stack setup
stack path
stack exec env
env
stack setup
env
. ./.bashrc
env
env | less
. ./.bashrc
env
env | less
$PATH
qq
rm -rf .stack/
cd /usr/local/bin
ll | grep "stack"
sudo rm stack
cd
rm .xboardrc 
wherseis stack
curl -sSL https://get.haskellstack.org/ | sh
stack setup
stack install haskell-language-server
htop
sudo apt install libicu-dev libncurses-dev libgmp-dev zlib1g-dev
stack ghc --version
stack ghc -version
stack ghc version
ghc version
stack ghc -- version
stack ghc -- --version
$PATH
stack --help
stack ghc -- --version
stack install haskell-language-server --resolver=9.2.5
stack install haskell-language-server --resolver=lts-20.8
stack install haskell-language-server
stack install haskell-language-server --resolver=lts-20.8
stack install ghcide-1.9.0.0
cc
cd Downloads/
git clone https://github.com/haskell/haskell-language-server
cd haskell-language-server/
stack install 
cd
ll .local/bin
haskell-language-server
cc
haskell-language-server-wrapper 
. ./.bashrc
haskell-language-server-wrapper 
. ./.bashrc
haskell-language-server-wrapper 
htop
python
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
editor
emacs .bashrc 
editor
EDITOR
$EDITOR
sudo apt search xft
sudo apt install  libpango1.0-dev 
sudo apt install  libpango1.0-0
ip addr
ip addr | awk "/state $1/ {print \$2}
ip addr | awk "/state $1/ {print \$2}"
ip addr | awk "/state $1/ {print \$2}" | grep -v "lo"
man grep
ip addr | awk "/state $1/ {print \$2}" | grep -v "lo" | head -n 1
ip addr | awk "/state $1/ {print \$2}" | grep -v "lo" | head -n 1 | sed 's/://'
ip addr | awk "/state "UP"/ {print \$2}" | grep -v "lo" | head -n 1 | sed 's/://'
xpdf
sudo apt install xpdf
cd Downloads/
man xpdf
xpdf Таблица\ Противопаразитарные\ препараты.pdf 
sudo apt remove  xpdf
cd Downloads/dotfiles_qbbr/
grep -rnw '.' -e 'pygments'
sudo apt install  bash-completion 
man df
df -color
df --color
df --help
qq
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
. $HOME/.cargo/env
qq
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
qq
stack exec env
cd Downloads/
sudo apt remove openjdk*
sudo apt autoremove
cd projects/work/
python
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd projects/work/
python
cd applications_table/refactoring/merge_files_toml/
cd core
python
cd core
python check_files.py 
cd ..
touch.check.py
touch check.py
python check.py 
cd projects/work/
python
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
python
python check.py 
python
python check.py 
python
python check.py 
cd ~/projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
cc
python check.py 
cc
python check.py 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
python check.py 
sudo apt update
sudo apt upgrade q
sudo apt upgrade 
emacs --version
cd projects/work/
python
cc
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
cc
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
python
python check.py 
cc
python check.py 
python
cc
./tprocessor 
cc
. ~/.bashrc 
htop
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
cd data/
python
cd ..
cd mobilization/inf_center_docx_to_xlsx/
./fprocessor 
ssh kirill@192.168.1.200:2299
ssh kirill@192.168.1.200
ssh server@192.168.1.200:2299
ssh server@192.168.1.200 -p 2299
ssh server@192.168.1.200:23331
ssh server@192.168.1.200 23331
ssh server@192.168.1.200 -p 23331
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
python test.py 
python check.py 
python test.py 
python check.py 
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
cd projects/work/applications_table/refactoring/merge_files_toml/
python check.py 
cd projects/work/applications_table/refactoring/merge_files_toml/
chmod +x checker 
ls -la
./checker 
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
cd projects/work/applications_table/refactoring/merge_files_toml/
./tprocessor 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
workon docx
./fprocessor 
sudo apt update 
sudo apt upgrade
дыидл
lsblk
df -h
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
cc
./checker 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
./tprocessor 
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
.f
./fprocessor 
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd projects/work/applications_table/refactoring/merge_files_toml/
./checker 
cc
./checker 
cd projects/work/applications_table/refactoring/merge_files_toml/
./tprocessor 
./checker 
./tprocessor 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
sudo apt update
sudo apt upgrade 
sudo apt autoremove 
sudo reboot 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd projects/work/applications_table/refactoring/rating_new/
python
sudo apt update
sudo apt upgrade 
sudo reboot
cd projects/work/mobilization/inf_center_docx_to_xlsx/
la
./fprocessor 
stack ghci
cd projects/haskell/
stack ghci Hello.hs
stack ghci
stack ghc version
stack ghc --version
stack ghci --version
stack -vwrsion
stack -version
stack --version
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
python
cd projects/work/applications_table/refactoring/merge_files_toml/
./tprocessor 
htop
cd projects/dotfiles/
git status 
ssh server@192.168.1.200 -p 23331
ssh kirill@192.168.1.200 -p 23331
ssh server@192.168.1.200 -p 23331
stack ghci
cd projects/haskell/
touch Func.hs
stack ghci
stack ghci Func.hs 
python
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
python
щлгдфк
okular
python
sudo apt update
sudo apt upgrade
sudo apt install racket
racket --version
cd projects/racket/
racket hello.rkt 
raco pkg install racket-langserver
racket --lib racket-langserver
htop
racket
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
cd projects/work/applications_table/refactoring/merge_files_toml/
./tprocessor 
htop
sudo apt update 
sudo apt upgrade 
sudo apt install sbcl
sudo apt install sbcl-doc sbcl-source
sbcl
whereis sbcl
cd projects/
./test.lisp
htop
stack ghci
ghci
cd projects/haskell/
ghc hello.hs 
./hello 
la
ghc hello.hs -o testprogram
la
./testprogram 
whereis ghc
ghci
ghc first_prog.hs -o email
./email 
cd .local/share/applications/
cd
cd /usr/share/
cd applications/
vim rxvt-unicode.desktop
ssh kirill@192.168.1.200 -p 23331
ssh kirill@192.168.1.200 P 23331
ssh kirill@192.168.1.200 -P 23331
ssh kirill@192.168.1.200 -p 23331
ssh server@192.168.1.200 -p 23331
sudo apt install fdupes
man fdupes
fdupes -rnS > dups.txt
fdupes -rnS /home/kirill > dups.txt
rustup self uninstall
sudo apt remove solargraph
sudo apt remove  texlive-*
sudo apt autoremove 
pip freeze
sudo apt remove  dia
sudo apt autoremove 
sudo apt remove pulseaudio
python
htop
sudo apt remove --purge postgresql
sudo apt remove --purge postgresql-*
sudo apt autoremove 
htop
cd projects/haskell/
ghc first_prog.hs
./first_prog 
rm first_prog.o first_prog.hi first_prog email 
rm -rf real/
ghci 
stack uninstall
rm -rf ~/.stack
cd
cd .local/bin/
rm haskell-language-server*
rm ghcide*
pip uninstall django-admin pymorhy
sudo pip uninstall django-admin pymorhy
pip3 uninstall django-admin 
rm django-admin 
rm pymorphy 
cd
cd .local/bin/
which stack
sudo rm /usr/local/bin/stack
which stack
sudo apt update
sudo apt upgrade 
htop
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup tui
. ./.bashrc 
ghcup tui
echo $PATH
haskell-language-server-wrapper
echo $PATH
. ./.bashrc
htop
cd projects/haskell
stack new test
cd test
vim stack.yaml 
vim test.cabal 
ghcup tui
stack config set system-ghc  true  --global
htop
cd ..
stack new test
cd test
vim stack.yaml 
stack ghc --version
ghc --version
ghcup
ghcup tui
echo $PATH
htop
echo $PYTHONPATH
htop
echo $PATH
ghc --version
htop
cat /etc/environment
sudo vim /etc/environment
echo $PATH
ghcup tui
which ghc
ghcup tui
htop
stack --version
racket hello.rkt 
ghcup
ghcup tui
stack --version
htop
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
systemctl --user disable --now emacs.service 
emacsclient 0t
emacsclient -t
raco pkg install racket-langserver
cd projects/racket/
racket hello.rkt 
htop
sudo vim /etc/environment
echo $PATH
htop
which racket
htop
sudo apt install mit-scheme
cd projects/
mkdir scheme
cd scheme/
touch test.scm
scheme test.scm 
scheme 
mit-scheme
scheme --load test.scm 
cd
man raco
raco pkg install sicp
htop
cd projects/work/convert_docx_list_to_excel/
workon
workon docx
python main.py 
pip install python-docx
pip install --upgrade pip
pip install pandas
python main.py 
cd projects/work/convert_docx_list_cards_to_excel/
. ./env/bin/activate
python main.py 
deactivate 
cd ..
cd convert_docx_list_to_excel/
. ./env/bin/activate
python main.py 
cd projects/work/convert_docx_list_cards_to_excel/
. ./env/bin/activate
python main.py 
pip freeze
pip install docx
pip install pandas
python3 main
python3 main.py 
cd projects/work/convert_docx_list_cards_to_excel/
. ./env/bin/activate
python main.py 
htop
cd projects/work/convert_docx_list_to_excel/
. ./env/bin/activate
python main.py 
deactivate 
python -m venv env
. ./env/bin/activate
pip install python-docs pandas
pip install python-docx pandas
python main.py 
pip install openpyxl
htop
cd projects/work/convert_docx_list_to_excel/
touch count.py
cd projects/work/convert_docx_list_to_excel/
. ./env/bin/activate
python main.py 
python count.py 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
рещз
htop
рещз
htop
cd projects/work/convert_docx_list_to_excel/
. ./env/bin/activate
python main_new.py 
cd projects/work/convert_docx_list_to_excel/
. ./env/bin/activate
python main.py 
deactivate 
. ./env/bin/activate
python main.py 
htop
cd projects/work/convert_docx_list_to_excel/
. ./env/bin/activate
python main.py 
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
прсш
gchi
echo $PATH
ghcup tui
stack
stack ghci
ghci
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./fprocessor 
sudo apt update
sudo apt upgrade 
cd projects/learning_python/ml_project_work/
. ./env/bin/activate
python 1.word_to_excel.py 
htop
cd projects/work/mobilization/inf_center_docx_to_xlsx/
./f
./fprocessor 
htop
cd projects/work/convert_docx_list_cards_to_excel/
. ./env/bin/activate
python main.py 
python concat_files.py 
cd ..
python test.py 
cd convert_docx_list_cards_to_excel/
python concat_files.py 
cd ..
python test.py 
cd convert_docx_list_cards_to_excel/
python concat_files.py 
cp exp.xlsx ../1.xlsx
cd ..
python test.py 
cd projects/work/convert_docx_list_cards_to_excel/
python test.py 
рещз
