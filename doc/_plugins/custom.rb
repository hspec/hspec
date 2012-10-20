require 'digest'

# use our custom version of pygments
Pygments.start(File.join(File.dirname(__FILE__), "../pygments"))

module Hspec
  module CustomFilters
    def runhaskell(input)
      digest = Digest::MD5.hexdigest(input)
      file   = File.join ".cache/runhaskell", digest
      puts "#{digest} (#{input})"

      if File.exists? file
        File.read file
      else
        r = `runhaskell -i../src -fobject-code -outputdircache/ghc #{input}`
        File.write file, r
        r
      end
    end
  end
end

Liquid::Template.register_filter(Hspec::CustomFilters)
