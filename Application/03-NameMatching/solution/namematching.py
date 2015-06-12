def calculate_chance(males_present, females_present, names):
    female_names = []
    male_names = []

    for name in names:
        if name.endswith("ss"):
            male_names.append(name)
        elif name.endswith("tta"):
            female_names.append(name)
    chance_males = (len(male_names) + 1) / float(males_present)
    chance_females = (len(female_names) + 1) / float(females_present)
    return max(chance_males, 1.0) * max(chance_females, 1.0)


def main():
    pass


if __name__ == "__main__":
    main()
