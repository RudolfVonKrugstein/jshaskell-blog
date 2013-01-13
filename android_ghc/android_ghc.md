Install android ndk!
If I do

    arm-linux-androideabi-gcc -print-sysroot

it outputs

    /tmp/ndk-andrewhsieh/build/toolchain/prefix/sysroot

so I do (I am sure there are other ways, but this works):

    alias ndk_setup=" mkdir -p /tmp/ndk-andrewhsieh/build/toolchain/prefix && ln -s $ANDROID_NDK/platforms/android-9/arch-arm /tmp/ndk-andrewhsieh/build/toolchain/prefix/sysroot"

Install iconv-android (static!): https://github.com/ironsteel/iconv-android
and ncurses: http://credentiality2.blogspot.de/2010/08/compile-ncurses-for-android.html
for the android ndk

checkout ghc:

    git clone http://darcs.haskell.org/ghc.git/
    ./sync-all --no-dph get

apply patch 2 and 3 from http://web.archiveorange.com/archive/v/j7U5dAvNego9GwzeIPFn (0001-Set-up-for-QNXNTO-OS.patch not needed).
These are needed for a cross compile.

Some more patches are needed!
To inform llvm of the target architecture, apply [mtriple.patch][mtriple].
I wonder if one should also add a data layout here???

In the ndk headers, winsize is defined in terminfo.h.
in libraries/haskeline: [add-terminfo.patch][add-terminfo]

For some reason in the android ndk build system when including termios.h, the offsetof macro is set to something, that can not be used as a constant expression (see http://stackoverflow.com/questions/14233983/storage-size-not-constant-when-using-offsetof-and-including-termios-h-with-andro)
in utils/hsc2hs: [offset.patch][offset]

There seem to be some more differences in the posix headers in the android ndk.
Some pw_gecos field of passwd struct is missing. We replace it with pw_name. Also telldir and seekdir are missing. We just remove those from the ffi.
In libraries/unix apply [unix-posix.patch][unix-posix]

Some ubuntu package one needs (hopefully complete list):

    sudo apt-get install autoconf libtool autotools-dev happy alex

Create a build.mk with

    INTEGER_LIBRARY = integer-simple

I also used

    BuildFlavour = quick

We need config.sub and config.guess, that accept accept out target:

    cp -av /usr/share/misc/config.sub ./
    cp -av /usr/share/misc/config.guess ./

Configure ghc (for now unregistered, if we do not do this the compiled haskell programs will segfault):

    perl boot
    ./configure --target=arm-linux-androideabi --enable-unregisterised

If you want to make a registerised build, leave out the --enable-unregisterised parameter. But for me, the executables created by the compiler with registerised build segfault.
Now compile:

    make

This will fail with:

    configure: error: /bin/bash ./config.sub arm-unknown-linux-android failed

so go do

    cd libraries/unix
    cp -av /usr/share/misc/config.sub ./
    cp -av /usr/share/misc/config.guess ./

again, this will fail with

    expectJust initTcInteractive

there is an easy fix. Type make again (it is a known problem)!
But this will fail again with something like this:

    Not in scope: type constructor or class `HTYPE_FLOAT'

So do

    <edit> ./libraries/base/include/HsBaseConfig.h

now replace

    /* Define to Haskell type for double */
    /* #undef HTYPE_DOUBLE */
    
    /* Define to Haskell type for float */
    /* #undef HTYPE_FLOAT */

with

    /* Define to Haskell type for double */
    #define HTYPE_DOUBLE Double
    
    /* Define to Haskell type for float */
    #define HTYPE_FLOAT Float

and do make again.

Now you can use ./inplace/bin/ghc-stage1 for compiling your haskell files to executable which can be exectued on the android device.

[add-terminfo]: https://raw.github.com/RudolfVonKrugstein/jshaskell-blog/master/android_ghc/patches/add-terminfo.patch
[mtriple]: https://raw.github.com/RudolfVonKrugstein/jshaskell-blog/master/android_ghc/patches/mtriple.patch
[offset]: https://raw.github.com/RudolfVonKrugstein/jshaskell-blog/master/android_ghc/patches/offset.patch
[unix-posix]: https://raw.github.com/RudolfVonKrugstein/jshaskell-blog/master/android_ghc/patches/unix-posix.patch
