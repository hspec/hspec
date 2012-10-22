require 'digest'

# use our custom version of pygments
Pygments.start(File.join(File.dirname(__FILE__), "../pygments"))

module Hspec
  module CustomFilters
    def runhaskell(cmd)
      cache  = ".cache/runhaskell"
      system "mkdir -p #{cache}"

      digest = Digest::MD5.hexdigest(cmd)
      file   = File.join cache, digest
      puts "#{digest} (#{cmd})"

      if File.exists? file
        File.read file
      else
        r = runhaskell_ cmd
        File.write file, r
        r
      end
    end

    def runhaskell_(cmd)
      `runhaskell -i../src -fobject-code -outputdir.cache/ghc #{cmd}`
    end
  end
end

Liquid::Template.register_filter(Hspec::CustomFilters)
