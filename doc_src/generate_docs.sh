mkdir -p ../doc
rm -rf   ../doc/*
cp ./support/index.html ../doc
cp ./support/style.css  ../doc
./support/generate_elements.rb    elements/*.yml
./support/generate_actions.rb     actions/*.yml
./support/generate_validators.rb  validators/*.yml
