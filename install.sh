echo 'Copying dofiles'
cp home/* ~/ -r
echo 'Install nebundle.vim:'
mkdir ~/.vim/bundle
git clone https://github.com/Shougo/neobundle.vim.git ~/.vim/bundle/neobundle.vim
