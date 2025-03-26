# Haskell implementation of Tree (recursive directory listing program)

This was a project developed by me after attending the "Foundamentals of Functional Programming" course by Prof. [Luca Abeni](https://github.com/lucabe72), at Sant'Anna School of Advanced Studies (Pisa), 2023-2024.


# How to build : 
-  clone repository
-  cd implementation_advanced
-  compile : 
    ghc -O2 jTree.hs -o haskell-tree



# Haskell implementation :
options : 
 -   --a : show hidden
 -   --d : only directories
 -   --L : follow symlinks and avoid cycles
 -   --f : fullpath prefix
 -   --c : colorize output (file vs dir vs symlink) 
 -   --p : show permissions
 -   --s : show sizes
 -   --level depth : show delpth children ( 0 == only target children )
 -   --output path : writes output directly to file (creates files if it does not exist)


# use example : 
output to terminal (comparisons against tree are visible in "comparison_capts" folder) : 

    ./haskell-tree --a --c --L --f ..
    
    ./haskell-tree --L --f --c --a --p --s --level 1 ../.. 



output to file (examples visible in "output_txt" folder) :


    ./haskell-tree --L --a --f --c --output output_txt/jtree_colourSafe.txt ../zzz_stress_tests/

    ./haskell-tree --L --a ../zzz_stress_tests/ > output_txt/jtree.txt
    
    tree -l -a ../zzz_stress_tests/ > output_txt/tree.txt
    
For output to file, either use "--output pathFile" option, or don't use "--c". <br> 
"--output pathFile" disables the colour option.


<br>


# Linux Reference : 
For reference, Linux implementation (standard of comparison) :
    <https://linux.die.net/man/1/tree>





by Jacopo Carlon



