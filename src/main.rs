extern crate clap;

use zoneparser::parse;

fn main() {
    let matches = clap::App::new("zoneparser")
        .version("0.0.1")
        .author("Bert JW Regeer")
        .about("Parses zone files")
        .arg(
            clap::Arg::with_name("zonefile")
                .required(true)
                .index(1)
                .help("The zonefile to parse")
                .takes_value(true),
        )
        .get_matches();

    let zonefile = matches.value_of("zonefile").unwrap();
    println!("Value for zonefile: {}", zonefile);

    let contents = std::fs::read_to_string(zonefile).expect("Unable to open the zonefile!");

    parse(&contents);
}
