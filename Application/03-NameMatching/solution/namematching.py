def calculate_chance(males_present, females_present, names):
    female_names = []
    male_names = []

    for name in names:
        if name.endswith("ss"):
            male_names.append(name)
        elif name.endswith("tta"):
            female_names.append(name)
    chance_males = 1.0 / max(len(male_names) - males_present, 1)
    chance_females = 1.0 / max(len(female_names) - females_present, 1)
    return chance_males * chance_females


def handle_io():
    try:
        print("Input:")
        males_present, females_present = map(int, raw_input().split())
        names = raw_input().split()
        print("Output:")
        chance = calculate_chance(males_present, females_present, names)
        print("{0}%".format(int(chance * 100)))
    except ValueError:
        print("Wrong input.")


def main():
    while True:
        try:
            handle_io()
        except KeyboardInterrupt:
            print("Bye.")
            break


if __name__ == "__main__":
    main()
